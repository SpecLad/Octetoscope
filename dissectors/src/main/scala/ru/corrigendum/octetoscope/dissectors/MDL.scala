/*
  This file is part of Octetoscope.
  Copyright (C) 2014-2015 Octetoscope contributors (see /AUTHORS.txt)

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
import ru.corrigendum.octetoscope.core.CommonConstraints._
import ru.corrigendum.octetoscope.core.CompoundDissectors._
import ru.corrigendum.octetoscope.core.PrimitiveDissectors._
import ru.corrigendum.octetoscope.core._
import ru.corrigendum.octetoscope.dissectors.Common.vector3

/*
  Quake models (*.mdl).

  Dissection is based on the Quake source code,
  available at <https://github.com/id-Software/Quake>.
*/

private[dissectors] object MDL extends MoleculeBuilderUnitDissector {
  val MagicBytes = Array[Byte]('I', 'D', 'P', 'O')

  // Quake II's struct mdl_t.
  private object Header extends MoleculeBuilderDissector[HeaderValue] {
    override def defaultWIP: HeaderValue = new HeaderValue

    override def dissectMB(input: Blob, offset: InfoSize, builder: MoleculeBuilder, value: HeaderValue): Unit = {
      val add = new SequentialAdder(input, offset, builder)

      val correctMagic = add("Identification", magic(MagicBytes, "IDPO")).isDefined
      if (!correctMagic) return

      val version = add("Version", sInt32L + equalTo(6, "ALIAS_VERSION"))
      if (version != 6) return

      add("Scale", vector3(float32L))
      add("Translation", vector3(float32L))
      add("Bounding radius", float32L)
      add("Eye position", vector3(float32L))

      value.numSkins = add.filtered("Number of skins", sInt32L)(positive)
      value.skinWidth = add.filtered("Skin width", sInt32L +? divisibleBy(4))(positive)
      value.skinHeight = add.filtered("Skin height", sInt32L +? noMoreThan(480, "MAX_LBM_HEIGHT"))(positive)

      value.numVertices = add.filtered("Number of vertices", sInt32L +? noMoreThan(2000, "MAXALIASVERTS"))(positive)
      value.numTriangles = add.filtered("Number of triangles", sInt32L)(positive)
      value.numFrames = add.filtered("Number of frames", sInt32L)(positive)

      add("Synchronization type", enum(sInt32L, Map(0 -> "ST_SYNC", 1 -> "ST_RAND")))
      add("Flags", bitField(32, Map(
        7L -> "EF_ROCKET", 6L -> "EF_GRENADE", 5L -> "EF_GIB",     4L -> "EF_ROTATE",
        3L -> "EF_TRACER", 2L -> "EF_ZOMGIB",  1L -> "EF_TRACER2", 0L -> "EF_TRACER3"), unnamedReason = "unused"))
      add("Size", float32L)
    }
  }

  private class HeaderValue {
    var numSkins: Option[Int] = None
    var skinWidth: Option[Int] = None
    var skinHeight: Option[Int] = None
    var numVertices: Option[Int] = None
    var numTriangles: Option[Int] = None
    var numFrames: Option[Int] = None
  }

  private class Skin(width: Int, height: Int) extends MoleculeBuilderUnitDissector {
    override def dissectMBU(input: Blob, offset: InfoSize, builder: MoleculeBuilder): Unit = {
      val add = new SequentialAdder(input, offset, builder)

      add("Type", enum(sInt32L, Map(0 -> "ALIAS_SKIN_SINGLE", 1 -> "ALIAS_SKIN_GROUP"))) match {
        case Some("ALIAS_SKIN_SINGLE") =>
          builder.setRepr("Single skin")
          add("Rows", array(height, "Row", array(width, "Color index", uInt8)))
        case _ => // Quake doesn't explicitly check for ALIAS_SKIN_GROUP
          for (numSkins <- add.filtered("Number of skins", sInt32L)(positive))
          {
            builder.setRepr("Skin group (%d skins)".format(numSkins))
            add("Skin intervals", array(numSkins, "Interval", float32L + positive))
            add("Skins", array(numSkins, "Skin", array(height, "Row", array(width, "Color index", uInt8))))
          }
      }
    }
  }

  // Quake's struct stvert_t.
  private object TexCoordPair extends MoleculeBuilderUnitDissector {
    override def dissectMBU(input: Blob, offset: InfoSize, builder: MoleculeBuilder): Unit = {
      val add = new SequentialAdder(input, offset, builder)

      /* The only bit that really makes sense to be set here is ALIAS_ONSEAM; the other bits
         are set by Quake itself during rendering (see R_AliasPreparePoints). However, Quake blindly
         uses the entire bit field as the initial value without checking that the other bits are
         unset or clearing them; thus those bits should take effect if set in the file. */
      val onSeamBits = add("On seam", bitField(32,
        Map(2L -> "ALIAS_ONSEAM", 3L -> "ALIAS_Z_CLIP", 4L -> "ALIAS_BOTTOM_CLIP",
            5L -> "ALIAS_RIGHT_CLIP", 6L -> "ALIAS_TOP_CLIP", 7L -> "ALIAS_LEFT_CLIP"),
        sbz = Set("ALIAS_Z_CLIP", "ALIAS_BOTTOM_CLIP", "ALIAS_RIGHT_CLIP", "ALIAS_TOP_CLIP", "ALIAS_LEFT_CLIP"),
        unnamedReason = "unused"))

      val sc = add.getContents("s", sInt32L)
      val tc = add.getContents("t", sInt32L)

      builder.setReprLazy("(%s, %s)%s".format(sc.repr, tc.repr,
        if (onSeamBits("ALIAS_ONSEAM")) " (on seam)" else ""))
    }
  }

  // Quake's struct dtriangle_t
  private class Triangle(numVertices: Int) extends MoleculeBuilderUnitDissector {
    val lessThanNumVertices = lessThan(numVertices, "the number of vertices")

    override def dissectMBU(input: Blob, offset: InfoSize, builder: MoleculeBuilder): Unit = {
      val add = new SequentialAdder(input, offset, builder)
      /* Quake occasionally compares these values for equality, so we
         don't just treat any non-zero value as Front. */
      val faceDirection = add("Face direction", enum(sInt32L, Map(0 -> "Back", 1 -> "Front"))).getOrElse("???")

      def formatSeq(elements: Seq[Any]) = elements.mkString("(", ", ", ")")

      val vic = add.getContents("Vertex indices",
        collectingArray(3, "Index", sInt32L + nonNegative + lessThanNumVertices, formatSeq))

      builder.setReprLazyO(vic.reprO.map("%s %s".format(faceDirection, _)))
    }
  }

  // Quake's struct trivertx_t.
  private object Vertex extends MoleculeBuilderUnitDissector {
    override def dissectMBU(input: Blob, offset: InfoSize, builder: MoleculeBuilder): Unit = {
      val add = new SequentialAdder(input, offset, builder)

      val coordsC = add.getContents("Coordinates", vector3(uInt8))
      val lniC = add.getContents("Light normal index", uInt8 + lessThan(162.toShort, "NUMVERTEXNORMALS"))

      builder.setReprLazyO(coordsC.reprO.map("%s | #%s".format(_, lniC.repr)))
    }
  }

  // Quake's struct daliasframe_t.
  private class SingleFrame(numVertices: Int) extends MoleculeBuilderUnitDissector {
    override def dissectMBU(input: Blob, offset: InfoSize, builder: MoleculeBuilder): Unit = {
      val add = new SequentialAdder(input, offset, builder)

      /* The bounding box corners are actually the same structure as
         Vertex, but let's show off the fact that the LNI is unused here. */
      add("Bounding box minimum", vector3(uInt8))
      add("Unused", uInt8)
      add("Bounding box maximum", vector3(uInt8))
      add("Unused", uInt8)

      val nameC = add.getContents("Name", asciiishZString(16))
      builder.setReprLazy(nameC.repr)

      add("Vertices", array(numVertices, "Vertex", Vertex))
    }
  }

  private class Frame(numVertices: Int) extends MoleculeBuilderUnitDissector {
    override def dissectMBU(input: Blob, offset: InfoSize, builder: MoleculeBuilder): Unit = {
      val add = new SequentialAdder(input, offset, builder)

      add("Type", enum(sInt32L, Map(0 -> "ALIAS_SINGLE", 1 -> "ALIAS_GROUP"))) match {
        case Some("ALIAS_SINGLE") =>
          val sfC = add.getContents("Single frame", new SingleFrame(numVertices))
          builder.setReprLazyO(sfC.reprO.map("Single frame %s".format(_)))
        case _ => // Quake doesn't explicitly check for ALIAS_GROUP
          // Quake's struct daliasgroup_t.
          for (numFrames <- add.filtered("Number of frames", sInt32L)(positive)) {
            builder.setRepr("Frame group (%d frames)".format(numFrames))

            add("Bounding box minimum", vector3(uInt8))
            add("Unused", uInt8)
            add("Bounding box maximum", vector3(uInt8))
            add("Unused", uInt8)

            add("Frame intervals", array(numFrames, "Interval", float32L + positive))
            add("Frames", array(numFrames, "Frame", new SingleFrame(numVertices)))
          }
      }
    }
  }

  override def dissectMBU(input: Blob, offset: InfoSize, builder: MoleculeBuilder): Unit = {
    builder.setRepr("Quake model")

    val add = new SequentialAdder(input, offset, builder)

    val header = add("Header", Header)

    for (numSkins <- header.numSkins; skinWidth <- header.skinWidth; skinHeight <- header.skinHeight) {
      add("Skins", array(numSkins, "Skin", new Skin(skinWidth, skinHeight)))

      for (numVertices <- header.numVertices) {
        add("Texture coordinates", array(numVertices, "Texture coordinate pair", TexCoordPair))

        for (numTriangles <- header.numTriangles) {
          add("Triangles", array(numTriangles, "Triangle", new Triangle(numVertices)))

          for (numFrames <- header.numFrames)
            add("Frames", array(numFrames, "Frame", new Frame(numVertices)))
        }
      }
    }
  }
}
