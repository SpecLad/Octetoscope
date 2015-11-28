/*
  This file is part of Octetoscope.
  Copyright (C) 2013-2015 Octetoscope contributors (see /AUTHORS.txt)

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

package ru.corrigendum.octetoscope.dissectors

import scala.util.control.Breaks._
import ru.corrigendum.octetoscope.core.CommonConstraints._
import ru.corrigendum.octetoscope.core.CompoundDissectors._
import ru.corrigendum.octetoscope.core.PrimitiveDissectors._
import ru.corrigendum.octetoscope.core.SpecialDissectors._
import ru.corrigendum.octetoscope.core._
import ru.corrigendum.octetoscope.dissectors.Common.vector3

/*
  Quake II models (*.md2).

  Dissection is based on the Quake II source code,
  available at <https://github.com/id-Software/Quake-2>.
*/

private[dissectors] object MD2 extends MoleculeBuilderUnitDissector {
  val MagicBytes = Array[Byte]('I', 'D', 'P', '2')

  // Quake II's struct dmdl_t.
  private object Header extends SimpleMoleculeBuilderDissector[HeaderValue] {
    override def defaultWIP = new HeaderValue

    override def dissectMB(context: DissectionContext,
                           offset: InfoSize,
                           builder: MoleculeBuilder,
                           value: HeaderValue): Unit = {
      val add = new SequentialAdder(context, offset, builder)

      val correctMagic = add("Identification", magic(MagicBytes, "IDP2")).isDefined
      if (!correctMagic) return

      val version = add("Version", sInt32L + equalTo(8, "ALIAS_VERSION"))
      if (version != 8) return

      add("Skin width", sInt32L + positive)
      add("Skin height", sInt32L + positive +? noMoreThan(480, "MAX_LBM_HEIGHT"))
      value.frameSize = add.filtered("Frame size", sInt32L)(nonNegative)
      value.numSkins = add.filtered("Number of skins", sInt32L)(nonNegative)
      value.numVertices = add.filtered("Number of vertices", sInt32L +? noMoreThan(2048, "MAX_VERTS"))(positive)
      value.numTexCoords = add.filtered("Number of texture coordinate pairs", sInt32L)(positive)
      value.numTriangles = add.filtered("Number of triangles", sInt32L)(positive)
      value.numOpenGL = add.filtered("Number of OpenGL command words", sInt32L)(positive)
      value.numFrames = add.filtered("Number of frames", sInt32L)(positive)

      value.offSkins = add.filtered("Offset of skins", sInt32L)(nonNegative)
      value.offTexCoords = add.filtered("Offset of texture coordinates", sInt32L)(nonNegative)
      value.offTriangles = add.filtered("Offset of triangles", sInt32L)(nonNegative)
      value.offFrames = add.filtered("Offset of frames", sInt32L)(nonNegative)
      value.offOpenGL = add.filtered("Offset of OpenGL commands", sInt32L)(nonNegative)

      val softSize = (context.softLimit - offset).bytes
      val fileSizeConstraint = if (softSize < Int.MaxValue)
        noMoreThan(softSize.toInt, "actual file size") else any

      value.fileSize = add.filtered("File size", sInt32L + fileSizeConstraint)(nonNegative)
    }
  }

  private class HeaderValue {
    var frameSize: Option[Int] = None
    var numSkins: Option[Int] = None
    var numVertices: Option[Int] = None
    var numTexCoords: Option[Int] = None
    var numTriangles: Option[Int] = None
    var numFrames: Option[Int] = None
    var numOpenGL: Option[Int] = None
    var offSkins: Option[Int] = None
    var offTexCoords: Option[Int] = None
    var offTriangles: Option[Int] = None
    var offFrames: Option[Int] = None
    var offOpenGL: Option[Int] = None
    var fileSize: Option[Int] = None
  }

  // Quake II's struct dstvert_t.
  private object TexCoordPair extends MoleculeBuilderUnitDissector {
    override def dissectMBU(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder): Unit = {
      val add = new SequentialAdder(context, offset, builder)
      val sc = add.getContents("s", sInt16L)
      val tc = add.getContents("t", sInt16L)
      builder.setReprLazy("(%s, %s)".format(sc.repr, tc.repr))
    }
  }

  // Quake II's struct dtriangle_t
  private class Triangle(numVertices: Option[Int], numTexCoords: Option[Int]) extends MoleculeBuilderUnitDissector {
    private[this] val vertexIndicesDissector = {
      val lessThanNumVertices = numVertices
        .map(num => if (num > Short.MaxValue) any else lessThan(num.toShort, "the number of vertices"))
        .getOrElse(any)
      collectingArray(3, "Index", sInt16L + nonNegative + lessThanNumVertices, formatSeq)
    }

    private[this] val texCoordIndicesDissector = {
      val lessThanNumTexCoords = numTexCoords
        .map(num => if (num > Short.MaxValue) any else lessThan(num.toShort, "the number of texture coordinate pairs"))
        .getOrElse(any)
      collectingArray(3, "Index", sInt16L + nonNegative + lessThanNumTexCoords, formatSeq)
    }

    override def dissectMBU(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder): Unit = {
      val add = new SequentialAdder(context, offset, builder)

      val vi = add("Vertex indices", vertexIndicesDissector)
      val ti = add("Texture coordinate pair indices", texCoordIndicesDissector)

      if (vi.length == 3 && ti.length == 3)
        builder.setReprLazy(formatSeq((vi, ti).zipped.map(_ + "/" + _)))
    }
  }

  // Quake II's struct dtrivertx_t
  private object Vertex extends MoleculeBuilderUnitDissector {
    override def dissectMBU(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder): Unit = {
      val add = new SequentialAdder(context, offset, builder)

      val coordsC = add.getContents("Coordinates", vector3(uInt8))
      val lniC = add.getContents("Light normal index", uInt8 + lessThan(162.toShort, "NUMVERTEXNORMALS"))

      builder.setReprLazyO(coordsC.reprO.map("%s | #%s".format(_, lniC.repr)))
    }
  }

  // Quake II's struct daliasframe_t
  private class Frame(numVertices: Option[Int]) extends MoleculeBuilderUnitDissector {
    private[this] val verticesDissector = numVertices.map(array(_, "Vertex", Vertex))

    override def dissectMBU(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder): Unit = {
      val add = new SequentialAdder(context, offset, builder)
      add("Scale", vector3(float32L))
      add("Translation", vector3(float32L))
      val nameC = add.getContents("Name", asciiishZString(16))
      builder.setReprLazy(nameC.repr)

      for (verticesDissector <- verticesDissector)
        add("Vertices", verticesDissector)
    }
  }

  private abstract sealed class OpenGLCommandTypeValue
  private sealed case class TriangleFan(numVertices: Int) extends OpenGLCommandTypeValue {
    override def toString: String = "Triangle Fan of " + numVertices
  }
  private sealed case class TriangleStrip(numVertices: Int) extends OpenGLCommandTypeValue {
    override def toString: String = "Triangle Strip of " + numVertices
  }
  private case object OpenGLEnd extends OpenGLCommandTypeValue {
    override def toString: String = "End"
  }

  private object OpenGLCommandType extends DissectorCR[Option[OpenGLCommandTypeValue]] {
    override def dissect(context: DissectionContext, offset: InfoSize): PieceCR[Option[OpenGLCommandTypeValue]] = {
      val intPiece = sInt32L.dissect(context, offset)
      val word = intPiece.contents.value

      if (word == Int.MinValue)
        return Atom(Bytes(4),
          new ContentsR(None) {
            override def repr: String = intPiece.contents.repr
          },
          Seq(Note(NoteSeverity.Failure, "too many vertices for a triangle fan")))

      val cmdType = if (word < 0) TriangleFan(-word) else if (word > 0) TriangleStrip(word) else OpenGLEnd
      val piece = Atom(Bytes(4), new ContentsR(Some(cmdType)) {
        override def repr: String = intPiece.contents.repr + " -> " + value.get.toString
      })

      if (word != 0 && Math.abs(word) < 3) piece.withNote(Note(NoteSeverity.Error, "too few vertices for a triangle"))
      else piece
    }
  }

  private class OpenGLVertex(numVertices: Option[Int]) extends MoleculeBuilderUnitDissector {
    private[this] val lessThanNumVertices = numVertices.map(lessThan(_, "number of vertices")).getOrElse(any)

    override def dissectMBU(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder): Unit = {
      val add = new SequentialAdder(context, offset, builder)
      val sc = add.getContents("Texture s", float32L)
      val tc = add.getContents("Texture t", float32L)
      val indC = add.getContents("Index", sInt32L + nonNegative + lessThanNumVertices)

      builder.setReprLazy("%s / (%s, %s)".format(indC.repr, sc.repr, tc.repr))
    }
  }

  private case class OpenGLCommandValue(var typ: Option[OpenGLCommandTypeValue])

  private class OpenGLCommand(totalNumVertices: Option[Int])
      extends SimpleMoleculeBuilderDissector[OpenGLCommandValue] {
    override def defaultWIP: OpenGLCommandValue = OpenGLCommandValue(None)

    override def dissectMB(context: DissectionContext,
                           offset: InfoSize,
                           builder: MoleculeBuilder,
                           value: OpenGLCommandValue): Unit = {
      val add = new SequentialAdder(context, offset, builder)
      value.typ = add("Type", OpenGLCommandType)

      for (typ <- value.typ) {
        builder.setReprLazy(typ.toString)

        val numVerts = typ match {
          case TriangleFan(n) => n
          case TriangleStrip(n) => n
          case OpenGLEnd => return
        }

        add("Vertices", array(numVerts, "Vertex", new OpenGLVertex(totalNumVertices)))
      }
    }
  }

  private class OpenGLCommands(numWords: Int, numVertices: Option[Int]) extends MoleculeBuilderUnitDissector {
    override def dissectMBU(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder): Unit = {
      val add = new SequentialAdder(context, offset, builder)
      builder.fixSize(Bytes(4L * numWords))

      val cmdDissector = new OpenGLCommand(numVertices)

      breakable {
        while (!add.limitReached) {
          val cmd = add("Command", cmdDissector)
          cmd.typ match {
            case Some(OpenGLEnd) => return
            case None => break
            case _ =>
          }
        }
      }

      builder.addNote(NoteSeverity.Error, "missing End command")
    }
  }

  override def dissectMBU(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder): Unit = {
    val add = new RandomAdder(context, offset, builder)
    val header = add("Header", Bytes(0), Header)

    for (offSkins <- header.offSkins; numSkins <- header.numSkins)
      add("Skins", Bytes(offSkins), array(numSkins, "Skin", asciiishZString(64)))

    for (offTexCoords <- header.offTexCoords; numTexCoords <- header.numTexCoords)
      add("Texture coordinates", Bytes(offTexCoords), array(numTexCoords, "Texture coordinate pair", TexCoordPair))

    for (offTriangles <- header.offTriangles; numTriangles <- header.numTriangles)
      add("Triangles", Bytes(offTriangles), array(numTriangles, "Triangle",
        new Triangle(header.numVertices, header.numTexCoords)))

    for (numFrames <- header.numFrames; offFrames <- header.offFrames; frameSize <- header.frameSize)
      add("Frames", Bytes(offFrames), array(numFrames, "Frame",
        fixedSize(new Frame(header.numVertices), Bytes(frameSize))))

    for (numOpenGL <- header.numOpenGL; offOpenGL <- header.offOpenGL)
      add("OpenGL commands", Bytes(offOpenGL),
        fixedSize(new OpenGLCommands(numOpenGL, header.numVertices), Bytes(4 * numOpenGL)))

    builder.setRepr("Quake II model")

    for (fileSize <- header.fileSize)
      builder.fixSize(Seq(Bytes(fileSize), Bytes(context.input.size) - offset).min)
  }
}
