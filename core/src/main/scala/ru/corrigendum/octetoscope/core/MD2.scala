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

    builder.addChild("Identification", AsciiString(4).dissect(input, Offset(0)))
    builder.addChild("Version", SInt32L.dissect(input, Offset(4)))

    builder.addChild("Skin width", SInt32L.dissect(input, Offset(8)))
    builder.addChild("Skin height", SInt32L.dissect(input, Offset(12)))
    builder.addChild("Frame size", SInt32L.dissect(input, Offset(16)))

    builder.addChild("Number of skins", SInt32L.dissect(input, Offset(20)))
    builder.addChild("Number of vertices", SInt32L.dissect(input, Offset(24)))
    builder.addChild("Number of texture coordinates", SInt32L.dissect(input, Offset(28)))
    builder.addChild("Number of triangles", SInt32L.dissect(input, Offset(32)))
    builder.addChild("Number of OpenGL commands", SInt32L.dissect(input, Offset(36)))
    builder.addChild("Number of frames", SInt32L.dissect(input, Offset(40)))

    builder.addChild("Offset of skins", SInt32L.dissect(input, Offset(44)))
    builder.addChild("Offset of texture coordinates", SInt32L.dissect(input, Offset(48)))
    builder.addChild("Offset of triangles", SInt32L.dissect(input, Offset(52)))
    builder.addChild("Offset of frames", SInt32L.dissect(input, Offset(56)))
    builder.addChild("Offset of OpenGL commands", SInt32L.dissect(input, Offset(60)))
    builder.addChild("File size", SInt32L.dissect(input, Offset(64)))

    builder.build()
  }

  override def dissect(input: Blob, offset: Offset): Piece = {

    Molecule(Some("MD2"), Seq(NamedPiece("Header", dissectHeader(input))))
  }
}
