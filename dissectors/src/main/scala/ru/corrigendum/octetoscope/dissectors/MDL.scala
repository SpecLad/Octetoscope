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
  private object Header extends MoleculeBuilderUnitDissector {
    override def dissectMBU(input: Blob, offset: InfoSize, builder: MoleculeBuilder[Unit]) {
      val add = new SequentialAdder(input, offset, builder)

      val correctMagic = add("Identification", magic(MagicBytes, "IDPO")).isDefined
      if (!correctMagic) return

      val version = add("Version", sInt32L + equalTo(6, "ALIAS_VERSION"))
      if (version != 6) return

      add("Scale", new Vector3(float32L))
      add("Translation", new Vector3(float32L))
      add("Bounding radius", float32L)
      add("Eye position", new Vector3(float32L))

      add("Number of skins", sInt32L + positive)
      add("Skin width", sInt32L + positive +? divisibleBy(4))
      add("Skin height", sInt32L + positive +? noMoreThan(480, "MAX_LBM_HEIGHT"))

      add.filtered("Number of vertices", sInt32L +? noMoreThan(2000, "MAXALIASVERTS"))(positive)
      add.filtered("Number of triangles", sInt32L)(positive)
      add.filtered("Number of frames", sInt32L)(positive)

      add("Synchronization type", enum(sInt32L, Map(0 -> "ST_SYNC", 1 -> "ST_RAND")))
      add("Flags", sInt32L) // TODO: change into flags
      add("Size", float32L)
    }
  }

  override def dissectMBU(input: Blob, offset: InfoSize, builder: MoleculeBuilder[Unit]) {
    builder.setRepr("Quake model")

    val add = new SequentialAdder(input, offset, builder)
    add("Header", Header)
  }
}
