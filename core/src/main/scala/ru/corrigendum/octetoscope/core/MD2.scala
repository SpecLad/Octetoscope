/*
  This file is part of Octetoscope.
  Copyright (C) 2013 Octetoscope contributors (see /AUTHORS.txt)

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

package ru.corrigendum.octetoscope.core

import ru.corrigendum.octetoscope.abstractinfra.Blob
import PrimitiveDissectors._

// MD2 dissection is based on the Quake II source code,
// available at <https://github.com/id-Software/Quake-2>.

object MD2 extends MoleculeBuilderDissector {
  private object Header extends MoleculeBuilderDissector {
    // Quake II's struct dmdl_t.
    override def dissect(input: Blob, offset: Offset, builder: MoleculeBuilder) {
      val adder = new SequentialAdder(input, offset, builder)

      adder("Identification", AsciiString(4))
      adder("Version", SInt32L)

      adder("Skin width", SInt32L)
      adder("Skin height", SInt32L)
      adder("Frame size", SInt32L)

      adder("Number of skins", SInt32L)
      adder("Number of vertices", SInt32L)
      adder("Number of texture coordinates", SInt32L)
      adder("Number of triangles", SInt32L)
      adder("Number of OpenGL commands", SInt32L)
      adder("Number of frames", SInt32L)

      adder("Offset of skins", SInt32L)
      adder("Offset of texture coordinates", SInt32L)
      adder("Offset of triangles", SInt32L)
      adder("Offset of frames", SInt32L)
      adder("Offset of OpenGL commands", SInt32L)
      adder("File size", SInt32L)
    }
  }

  override def dissect(input: Blob, offset: Offset, builder: MoleculeBuilder) {
    val (header, _) = Header.dissect(input, offset)
    builder.addChild("Header", Offset(), header)
    builder.setRepr("MD2")
  }
}
