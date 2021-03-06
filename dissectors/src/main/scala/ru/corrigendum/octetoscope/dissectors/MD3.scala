/*
  This file is part of Octetoscope.
  Copyright (C) 2015 Octetoscope contributors (see /AUTHORS.txt)

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

import ru.corrigendum.octetoscope.core.CommonConstraints._
import ru.corrigendum.octetoscope.core.CompoundDissectors._
import ru.corrigendum.octetoscope.core.PrimitiveDissectors._
import ru.corrigendum.octetoscope.core._

/*
  Quake III models (*.md3).

  Dissection is based on the Quake III Arena source code,
  available at <https://github.com/id-Software/Quake-III-Arena>.
*/

private[dissectors] object MD3 extends MoleculeBuilderUnitDissector {
  val MagicBytes = Array[Byte]('I', 'D', 'P', '3')

  private class HeaderValue {
    var nameC: Option[ContentsR[Any]] = None
    var numFrames: Option[Int] = None
    var numTags: Option[Int] = None
    var numSurfaces: Option[Int] = None
    var offFrames: Option[Int] = None
    var offTags: Option[Int] = None
    var offSurfaces: Option[Int] = None
    var fileSize: Option[Int] = None
  }

  // Quake III's md3Header_t
  private object Header extends SimpleMoleculeBuilderDissector[HeaderValue] {
    override def defaultWIP: HeaderValue = new HeaderValue

    override def dissectMB(context: DissectionContext, offset: InfoSize,
                           builder: MoleculeBuilder, wip: HeaderValue): Unit = {
      val add = new SequentialAdder(context, offset, builder)

      val correctMagic = add("Identification", magic(MagicBytes, "IDP3")).isDefined
      if (!correctMagic) {
        builder.addNote(NoteSeverity.Failure, "incorrect identification")
        return
      }

      val version = add("Version", sInt32L + equalTo(15, "MD3_VERSION"))
      if (version != 15) {
        builder.addNote(NoteSeverity.Failure, "unsupported version")
        return
      }

      wip.nameC = Some(add.getContents("Name", asciiishZString(64)))
      add("Flags", bitField(32, Map.empty, unnamedReason = "unused"))

      wip.numFrames = add.filtered("Number of frames",
        sInt32L +? noMoreThan(1024, "MD3_MAX_FRAMES") + positive)(nonNegative)
      wip.numTags = add.filtered("Number of tags", sInt32L +? noMoreThan(16, "MD3_MAX_TAGS"))(nonNegative)
      wip.numSurfaces = add.filtered("Number of surfaces", sInt32L +? noMoreThan(32, "MD3_MAX_SURFACES"))(nonNegative)

      // though it is unused, being a count it presumably must still be >= 0
      add("Number of skins (unused)", sInt32L + nonNegative)

      wip.offFrames = add.filtered("Offset of frames", sInt32L)(nonNegative)
      wip.offTags = add.filtered("Offset of tags", sInt32L)(nonNegative)
      wip.offSurfaces = add.filtered("Offset of surfaces", sInt32L)(nonNegative)

      val softSize = (context.softLimit - offset).bytes
      val fileSizeConstraint = if (softSize < Int.MaxValue)
        noMoreThan(softSize.toInt, "actual file size") else any

      wip.fileSize = add.filtered("File size", sInt32L + fileSizeConstraint)(nonNegative)
    }
  }

  // Quake III's md3Frame_t
  private object Frame extends MoleculeBuilderUnitDissector {
    override def dissectMBU(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder): Unit = {
      val add = new SequentialAdder(context, offset, builder)

      add("Lower bound", Common.vector3(float32L))
      add("Upper bound", Common.vector3(float32L))
      add("Local origin", Common.vector3(float32L))
      add("Radius", float32L)
      val nameC = add.getContents("Name", asciiishZString(16))

      builder.setReprLazy(nameC.repr)
    }
  }

  // Quake III's md3Tag_t
  private object Tag extends MoleculeBuilderUnitDissector {
    override def dissectMBU(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder): Unit = {
      val add = new SequentialAdder(context, offset, builder)

      val nameC = add.getContents("Name", asciiishZString(64))
      builder.setReprLazy(nameC.repr)

      add("Origin", Common.vector3(float32L))
      add("X axis", Common.vector3(float32L))
      add("Y axis", Common.vector3(float32L))
      add("Z axis", Common.vector3(float32L))
    }
  }

  private class SurfaceHeaderValue {
    var nameC: Option[ContentsR[Any]] = None
    var numFrames: Option[Int] = None
    var numShaders: Option[Int] = None
    var numVertices: Option[Int] = None
    var numTriangles: Option[Int] = None
    var offTriangles: Option[Int] = None
    var offShaders: Option[Int] = None
    var offTexCoords: Option[Int] = None
    var offVertices: Option[Int] = None
    var surfaceSize: Option[Int] = None
  }

  // Quake III's md3Surface_t
  private class SurfaceHeader(expectedNumFrames: Option[Int])
      extends SimpleMoleculeBuilderDissector[SurfaceHeaderValue] {
    override def defaultWIP: SurfaceHeaderValue = new SurfaceHeaderValue

    override def dissectMB(context: DissectionContext, offset: InfoSize,
                           builder: MoleculeBuilder, wip: SurfaceHeaderValue): Unit = {
      val add = new SequentialAdder(context, offset, builder)

      add("Identification (unused)", opaque(Bytes(4)))
      wip.nameC = Some(add.getContents("Name", asciiishZString(64)))
      add("Flags", bitField(32, Map.empty, unnamedReason = "unused"))

      val nfc = expectedNumFrames.fold[Constraint[Int]](any)(equalTo(_, "global frame count"))
      wip.numFrames = add.filtered("Number of frames", sInt32L + nfc)(nonNegative)

      wip.numShaders = add.filtered("Number of shaders", sInt32L +? noMoreThan(256, "MD3_MAX_SHADERS"))(nonNegative)
      wip.numVertices = add.filtered("Number of vertices",
        sInt32L +? noMoreThan(1000, "SHADER_MAX_VERTEXES"))(nonNegative)
      wip.numTriangles = add.filtered("Number of triangles",
        sInt32L +? noMoreThan(2000, "SHADER_MAX_INDEXES / 3"))(nonNegative)
      wip.offTriangles = add.filtered("Offset of triangles", sInt32L)(nonNegative)
      wip.offShaders = add.filtered("Offset of shaders", sInt32L)(nonNegative)
      wip.offTexCoords = add.filtered("Offset of texture coordinates", sInt32L)(nonNegative)
      wip.offVertices = add.filtered("Offset of vertices", sInt32L)(nonNegative)

      val remaining = (context.softLimit - offset).bytes
      val sizeConstraint = if (remaining < Int.MaxValue)
        noMoreThan(remaining.toInt, "distance to EOF") else any
      wip.surfaceSize = add.filtered("Surface size", sInt32L + sizeConstraint)(nonNegative)
    }
  }

  // Quake III's md3Shader_t
  private object Shader extends MoleculeBuilderUnitDissector {
    override def dissectMBU(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder): Unit = {
      val add = new SequentialAdder(context, offset, builder)

      val nameC = add.getContents("Name", asciiishZString(64))
      builder.setRepr(nameC.repr)
      add("Index (unused)", sInt32L)
    }
  }

  // Quake III's md3St_t
  private object TexCoordPair extends MoleculeBuilderUnitDissector {
    override def dissectMBU(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder): Unit = {
      val add = new SequentialAdder(context, offset, builder)
      val sc = add.getContents("s", float32L)
      val tc = add.getContents("t", float32L)
      builder.setReprLazy("(%s, %s)".format(sc.repr, tc.repr))
    }
  }

  // Quake III's md3XyzNormal_t
  private object Vertex extends MoleculeBuilderUnitDissector {
    override def dissectMBU(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder): Unit = {
      val add = new SequentialAdder(context, offset, builder)

      val coordsC = add.getContents("Coordinates", Common.vector3(sInt16L))
      val lngC = add.getContents("Normal longitude", uInt8)
      val latC = add.getContents("Normal latitude", uInt8)

      builder.setReprLazyO(coordsC.reprO.map("%s | (%s, %s)".format(_, latC.repr, lngC.repr)))
    }
  }

  private class Surface(expectedNumFrames: Option[Int]) extends MoleculeBuilderUnitDissector {
    override def dissectMBU(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder): Unit = {
      val add = new RandomAdder(context, offset, builder)
      val header = add("Header", Bytes(0), new SurfaceHeader(expectedNumFrames))

      builder.setReprLazyO(header.nameC.map(_.repr))

      for (surfaceSize <- header.surfaceSize)
        builder.fixSize(Seq(Bytes(surfaceSize), Bytes(context.input.size) - offset).min)

      val lessThanNumVerices = header.numVertices.fold[Constraint[Int]](any)(lessThan(_, "number of vertices"))

      for (numTriangles <- header.numTriangles; offTriangles <- header.offTriangles)
        add("Triangles", Bytes(offTriangles), array(numTriangles, "Triangle",
          collectingArray(3, "Vertex index", sInt32L + nonNegative + lessThanNumVerices, formatSeq)))

      for (numShaders <- header.numShaders; offShaders <- header.offShaders)
        add("Shaders", Bytes(offShaders), array(numShaders, "Shader", Shader))

      for (numVertices <- header.numVertices; offTexCoords <- header.offTexCoords)
        add("Texture coordinates", Bytes(offTexCoords), array(numVertices, "Texture coordinate pair", TexCoordPair))

      for (numFrames <- header.numFrames; offFrames <- header.offVertices; numVertices <- header.numVertices)
        add("Vertices", Bytes(offFrames), array(numFrames, "Frame", array(numVertices, "Vertex", Vertex)))
    }
  }

  override def dissectMBU(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder): Unit = {
    val add = new RandomAdder(context, offset, builder)
    val header = add("Header", Bytes(0), Header)

    builder.setReprLazy(header.nameC.fold("Quake III model")("Quake III model " + _.repr))

    for (fileSize <- header.fileSize)
      builder.fixSize(Seq(Bytes(fileSize), Bytes(context.input.size) - offset).min)

    for (numFrames <- header.numFrames; offFrames <- header.offFrames)
      add("Frames", Bytes(offFrames), array(numFrames, "Frame", Frame))

    for (numTags <- header.numTags; offTags <- header.offTags)
      add("Tags", Bytes(offTags), array(numTags, "Tag", Tag))

    for (numSurfaces <- header.numSurfaces; offSurfaces <- header.offSurfaces)
      add("Surfaces", Bytes(offSurfaces), array(numSurfaces, "Surface", new Surface(header.numFrames)))
  }
}
