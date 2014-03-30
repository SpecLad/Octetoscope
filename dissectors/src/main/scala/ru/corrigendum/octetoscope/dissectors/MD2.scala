/*
  This file is part of Octetoscope.
  Copyright (C) 2013-2014 Octetoscope contributors (see /AUTHORS.txt)

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

import ru.corrigendum.octetoscope.abstractinfra.Blob
import java.util.Locale
import ru.corrigendum.octetoscope.core._
import ru.corrigendum.octetoscope.core.PrimitiveDissectors._
import ru.corrigendum.octetoscope.core.CommonConstraints._
import ru.corrigendum.octetoscope.core.CompoundDissectors._
import Common.Vector3

/*
  Quake II models (*.md2).

  Dissection is based on the Quake II source code,
  available at <https://github.com/id-Software/Quake-2>.
*/

object MD2 extends MoleculeBuilderUnitDissector {
  // Quake II's struct dmdl_t.
  private object Header extends MoleculeBuilderDissector[HeaderValue] {
    private val magicBytes = Array[Byte]('I', 'D', 'P', '2')

    override def defaultValue = new HeaderValue

    override def dissectMB(input: Blob, offset: InfoSize, builder: MoleculeBuilder, value: HeaderValue) {
      val add = new SequentialAdder(input, offset, builder)

      val correctMagic = add("Identification", magic(magicBytes, "IDP2")).isDefined
      if (!correctMagic) return

      val correctVersion = add("Version", sInt32L +! equalTo(8, "ALIAS_VERSION")).isDefined
      if (!correctVersion) return

      /*
        Quake II doesn't check every field below to be positive,
        but the dissector does it anyway, because negative values make
        no sense for them.

        The "no more than" checks only produce warnings, because they're
        engine limitations and not imposed by the format itself. Source
        ports, for example, could omit these checks and support a wider
        range of values.
      */

      add("Skin width", sInt32L +! positive)
      add("Skin height", sInt32L +! positive +? noMoreThan(480, "MAX_LBM_HEIGHT"))
      value.frameSize = add("Frame size", sInt32L +! nonNegative)

      value.numSkins = add("Number of skins", sInt32L +! nonNegative)
      value.numVertices = add("Number of vertices", sInt32L +! positive +? noMoreThan(2048, "MAX_VERTS"))
      value.numTexCoords = add("Number of texture coordinate pairs", sInt32L +! positive)
      value.numTriangles = add("Number of triangles", sInt32L +! positive)
      value.numOpenGL = add("Number of OpenGL command words", sInt32L +! positive)
      value.numFrames = add("Number of frames", sInt32L +! positive)

      value.offSkins = add("Offset of skins", sInt32L +! nonNegative)
      value.offTexCoords = add("Offset of texture coordinates", sInt32L +! nonNegative)
      value.offTriangles = add("Offset of triangles", sInt32L +! nonNegative)
      value.offFrames = add("Offset of frames", sInt32L +! nonNegative)
      value.offOpenGL = add("Offset of OpenGL commands", sInt32L +! nonNegative)

      val fileSizeConstraint = if (input.size - offset.bytes <= Int.MaxValue)
        noMoreThan((input.size - offset.bytes).toInt, "actual file size") else any

      value.fileSize = add("File size", sInt32L +! nonNegative +! fileSizeConstraint)
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
    override def dissectMBU(input: Blob, offset: InfoSize, builder: MoleculeBuilder) {
      val add = new SequentialAdder(input, offset, builder)
      val s = add("s", sInt16L)
      val t = add("t", sInt16L)
      builder.setRepr("(%d, %d)".format(s, t))
    }
  }

  // Quake II's struct dtriangle_t
  private class Triangle(numVertices: Option[Int], numTexCoords: Option[Int]) extends MoleculeBuilderUnitDissector {
    val validVertexIndex = numVertices
      .map(num => if (num > Short.MaxValue) any else lessThan(num.toShort, "the number of vertices"))
      .getOrElse(any)
    val validTexCoordIndex = numTexCoords
      .map(num => if (num > Short.MaxValue) any else lessThan(num.toShort, "the number of texture coordinate pairs"))
      .getOrElse(any)

    override def dissectMBU(input: Blob, offset: InfoSize, builder: MoleculeBuilder) {
      val add = new SequentialAdder(input, offset, builder)
      def formatSeq(elements: Seq[Any]) = elements.mkString("(", ", ", ")")

      val vi = add("Vertex indices", collectingArray(3, "Index",
        sInt16L + nonNegative + validVertexIndex, formatSeq))
      val ti = add("Texture coordinate pair indices", collectingArray(3, "Index",
        sInt16L + nonNegative + validTexCoordIndex, formatSeq))

      if (vi.length == 3 && ti.length == 3)
        builder.setRepr(formatSeq((vi, ti).zipped.map(_ + "/" + _)))
    }
  }

  // Quake II's struct dtrivertx_t
  private object FrameVertex extends MoleculeBuilderUnitDissector {
    override def dissectMBU(input: Blob, offset: InfoSize, builder: MoleculeBuilder) {
      val add = new SequentialAdder(input, offset, builder)

      val coords = add("Coordinates", new Vector3(uInt8))
      val lni = add("Light normal index", uInt8 +! lessThan(162.toShort, "NUMVERTEXNORMALS"))

      for (x <- coords.x; y <- coords.y; z <- coords.z; lni <- lni)
        builder.setRepr("(%s, %s, %s) | #%s".format(x, y, z, lni))
    }
  }

  // Quake II's struct daliasframe_t
  private class Frame(frameSize: Int, numVertices: Option[Int]) extends MoleculeBuilderUnitDissector {
    override def dissectMBU(input: Blob, offset: InfoSize, builder: MoleculeBuilder) {
      builder.fixSize(Bytes(frameSize))
      val add = new SequentialAdder(input, offset, builder)
      add("Scale", new Vector3(float32L))
      add("Translation", new Vector3(float32L))
      builder.setRepr("\"" + add("Name", asciiZString(16)) + "\"")

      for (numVertices <- numVertices)
        add("Vertices", array(numVertices, "Vertex", FrameVertex))
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

  private object OpenGLCommandType extends DissectorO[OpenGLCommandTypeValue] {
    override def dissectO(input: Blob, offset: InfoSize): (Piece, Option[OpenGLCommandTypeValue]) = {
      val word = readInt32L(input, offset)
      if (word == Int.MinValue)
        return (
          Atom(Bytes(4), Some(word.toString), Seq(PieceNote(PieceQuality.Broken, "too many vertices for a triangle fan"))),
          None)

      val value = if (word < 0) TriangleFan(-word) else if (word > 0) TriangleStrip(word) else OpenGLEnd
      val piece = Atom(Bytes(4), Some("%d -> %s".format(word, value)))

      if (word != 0 && Math.abs(word) < 3)
        (piece.withNote(PieceNote(PieceQuality.Bad, "too few vertices for a triangle")), Some(value))
      else
        (piece, Some(value))
    }
  }

  private class OpenGLVertex(numVertices: Option[Int]) extends MoleculeBuilderUnitDissector {
    val validVertexIndex = numVertices.map(lessThan(_, "number of vertices")).getOrElse(any)

    override def dissectMBU(input: Blob, offset: InfoSize, builder: MoleculeBuilder) {
      val add = new SequentialAdder(input, offset, builder)
      val s = add("Texture s", float32L)
      val t = add("Texture t", float32L)
      val ind = add("Index", sInt32L +! nonNegative +! validVertexIndex)

      for (ind <- ind)
        builder.setRepr("%d / (%f, %f)".formatLocal(Locale.ROOT, ind, s, t))
    }
  }

  private case class OpenGLCommandValue(var typ: Option[OpenGLCommandTypeValue])

  private class OpenGLCommand(totalNumVertices: Option[Int]) extends MoleculeBuilderDissector[OpenGLCommandValue] {
    override def defaultValue: OpenGLCommandValue = OpenGLCommandValue(None)

    override def dissectMB(input: Blob, offset: InfoSize, builder: MoleculeBuilder, value: OpenGLCommandValue) {
      val add = new SequentialAdder(input, offset, builder)
      value.typ = add("Type", OpenGLCommandType)

      for (typ <- value.typ) {
        builder.setRepr(typ.toString)

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
    override def dissectMBU(input: Blob, offset: InfoSize, builder: MoleculeBuilder) {
      val add = new SequentialAdder(input, offset, builder)
      builder.fixSize(Bytes(4L * numWords))

      var dissectedWords = 0
      val cmdDissector = new OpenGLCommand(numVertices)

      while (dissectedWords < numWords) {
        val cmd = add("Command", cmdDissector)
        cmd.typ match {
          case Some(TriangleFan(n)) => dissectedWords += 1 + 3 * n
          case Some(TriangleStrip(n)) => dissectedWords += 1 + 3 * n
          case Some(OpenGLEnd) => return
          case None => dissectedWords = Int.MaxValue
        }
      }

      builder.addNote(PieceQuality.Bad, "missing End command")
    }
  }

  override def dissectMBU(input: Blob, offset: InfoSize, builder: MoleculeBuilder) {
    val add = new RandomAdder(input, offset, builder)
    val header = add("Header", Bytes(0), Header)

    for (offSkins <- header.offSkins; numSkins <- header.numSkins)
      add("Skins", Bytes(offSkins), array(numSkins, "Skin", asciiZString(64)))

    for (offTexCoords <- header.offTexCoords; numTexCoords <- header.numTexCoords)
      add("Texture coordinates", Bytes(offTexCoords), array(numTexCoords, "Texture coordinate pair", TexCoordPair))

    for (offTriangles <- header.offTriangles; numTriangles <- header.numTriangles)
      add("Triangles", Bytes(offTriangles), array(numTriangles, "Triangle",
        new Triangle(header.numVertices, header.numTexCoords)))

    for (numFrames <- header.numFrames; offFrames <- header.offFrames; frameSize <- header.frameSize)
      add("Frames", Bytes(offFrames), array(numFrames, "Frame", new Frame(frameSize, header.numVertices)))

    for (numOpenGL <- header.numOpenGL; offOpenGL <- header.offOpenGL)
      add("OpenGL commands", Bytes(offOpenGL), new OpenGLCommands(numOpenGL, header.numVertices))

    builder.setRepr("Quake II model")

    for (fileSize <- header.fileSize)
      builder.fixSize(Bytes(fileSize))
  }
}
