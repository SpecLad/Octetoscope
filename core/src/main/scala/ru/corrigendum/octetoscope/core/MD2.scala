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

object MD2 extends Dissector {
  private def dissectHeader(input: Blob): Piece = {
    // Quake II's struct dmdl_t.

    val builder = new MoleculeBuilder

    builder.addChild("Identification", Offset(), AsciiString(4).dissect(input, Offset(0)))
    builder.addChild("Version", Offset(), SInt32L.dissect(input, Offset(4)))

    builder.addChild("Skin width", Offset(), SInt32L.dissect(input, Offset(8)))
    builder.addChild("Skin height", Offset(), SInt32L.dissect(input, Offset(12)))
    builder.addChild("Frame size", Offset(), SInt32L.dissect(input, Offset(16)))

    builder.addChild("Number of skins", Offset(), SInt32L.dissect(input, Offset(20)))
    builder.addChild("Number of vertices", Offset(), SInt32L.dissect(input, Offset(24)))
    builder.addChild("Number of texture coordinates", Offset(), SInt32L.dissect(input, Offset(28)))
    builder.addChild("Number of triangles", Offset(), SInt32L.dissect(input, Offset(32)))
    builder.addChild("Number of OpenGL commands", Offset(), SInt32L.dissect(input, Offset(36)))
    builder.addChild("Number of frames", Offset(), SInt32L.dissect(input, Offset(40)))

    builder.addChild("Offset of skins", Offset(), SInt32L.dissect(input, Offset(44)))
    builder.addChild("Offset of texture coordinates", Offset(), SInt32L.dissect(input, Offset(48)))
    builder.addChild("Offset of triangles", Offset(), SInt32L.dissect(input, Offset(52)))
    builder.addChild("Offset of frames", Offset(), SInt32L.dissect(input, Offset(56)))
    builder.addChild("Offset of OpenGL commands", Offset(), SInt32L.dissect(input, Offset(60)))
    builder.addChild("File size", Offset(), SInt32L.dissect(input, Offset(64)))

    builder.build()
  }

  override def dissect(input: Blob, offset: Offset): Piece = {
    val header = dissectHeader(input)
    Molecule(header.length, Some("MD2"), Seq(SubPiece("Header", offset, header)))
  }
}
