/*
  This file is part of Octetoscope.
  Copyright (C) 2014 Octetoscope contributors (see /AUTHORS.txt)

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

import ru.corrigendum.octetoscope.core._
import ru.corrigendum.octetoscope.abstractinfra.Blob
import ru.corrigendum.octetoscope.core.PrimitiveDissectors._
import ru.corrigendum.octetoscope.core.CompoundDissectors._
import ru.corrigendum.octetoscope.core.CommonConstraints._
import ru.corrigendum.octetoscope.dissectors.Common.Vector3

/*
  Quake models (*.mdl).

  Dissection is based on the Quake source code,
  available at <https://github.com/id-Software/Quake>.
*/

private[dissectors] object MDL extends MoleculeBuilderUnitDissector {
  val MagicBytes = Array[Byte]('I', 'D', 'P', 'O')

  // Quake II's struct mdl_t.
  private object Header extends MoleculeBuilderDissector[HeaderValue] {
    override def defaultValue: HeaderValue = new HeaderValue

    override def dissectMB(input: Blob, offset: InfoSize, builder: MoleculeBuilder[HeaderValue], value: HeaderValue) {
      val add = new SequentialAdder(input, offset, builder)

      val correctMagic = add("Identification", magic(MagicBytes, "IDPO")).isDefined
      if (!correctMagic) return

      val version = add("Version", sInt32L + equalTo(6, "ALIAS_VERSION"))
      if (version != 6) return

      add("Scale", new Vector3(float32L))
      add("Translation", new Vector3(float32L))
      add("Bounding radius", float32L)
      add("Eye position", new Vector3(float32L))

      value.numSkins = add.filtered("Number of skins", sInt32L)(positive)
      value.skinWidth = add.filtered("Skin width", sInt32L +? divisibleBy(4))(positive)
      value.skinHeight = add.filtered("Skin height", sInt32L +? noMoreThan(480, "MAX_LBM_HEIGHT"))(positive)

      add.filtered("Number of vertices", sInt32L +? noMoreThan(2000, "MAXALIASVERTS"))(positive)
      add.filtered("Number of triangles", sInt32L)(positive)
      add.filtered("Number of frames", sInt32L)(positive)

      add("Synchronization type", enum(sInt32L, Map(0 -> "ST_SYNC", 1 -> "ST_RAND")))
      add("Flags", bitField(32, Map(
        7L -> "EF_ROCKET", 6L -> "EF_GRENADE", 5L -> "EF_GIB",     4L -> "EF_ROTATE",
        3L -> "EF_TRACER", 2L -> "EF_ZOMGIB",  1L -> "EF_TRACER2", 0L -> "EF_TRACER3")))
      add("Size", float32L)
    }
  }

  private class HeaderValue {
    var numSkins: Option[Int] = None
    var skinWidth: Option[Int] = None
    var skinHeight: Option[Int] = None
  }

  private class Skin(width: Int, height: Int) extends MoleculeBuilderUnitDissector {
    override def dissectMBU(input: Blob, offset: InfoSize, builder: MoleculeBuilder[Unit]) {
      val add = new SequentialAdder(input, offset, builder)

      add("Type", enum(sInt32L, Map(0 -> "ALIAS_SKIN_SINGLE", 1 -> "ALIAS_SKIN_GROUP"))) match {
        case Some("ALIAS_SKIN_SINGLE") =>
          builder.setRepr("Single skin")
          add("Rows", array(height, "Row", array(width, "Color index", uInt8)))
        case Some("ALIAS_SKIN_GROUP") =>
          for (numSkins <- add.filtered("Number of skins", sInt32L)(positive))
          {
            builder.setRepr("Skin group (%d skins)".format(numSkins))
            add("Skin intervals", array(numSkins, "Interval", float32L + positive))
            add("Skins", array(numSkins, "Skin", array(height, "Row", array(width, "Color index", uInt8))))
          }
        case _ =>
      }
    }
  }

  override def dissectMBU(input: Blob, offset: InfoSize, builder: MoleculeBuilder[Unit]) {
    builder.setRepr("Quake model")

    val add = new SequentialAdder(input, offset, builder)

    val header = add("Header", Header)

    for (numSkins <- header.numSkins; skinWidth <- header.skinWidth; skinHeight <- header.skinHeight)
      add("Skins", array(numSkins, "Skin", new Skin(skinWidth, skinHeight)))
  }
}
