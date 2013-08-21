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

import java.nio.charset.StandardCharsets
import ru.corrigendum.octetoscope.abstractinfra.Blob

// MD2 dissection is based on the Quake II source code,
// available at <https://github.com/id-Software/Quake-2>.

object MD2 extends Dissector {
  private def dissectString(input: Blob): Piece = {
    Atom(Some(new String(input.toArray, StandardCharsets.US_ASCII)))
  }

  private def unsign(b: Byte): Int = if(b >= 0) b else 256 + b

  private def dissectInt(input: Blob): Piece = {
    Atom(Some(
      (unsign(input(0))
      | (unsign(input(1)) << 8)
      | (unsign(input(2)) << 16)
      | (unsign(input(3)) << 24)).toString))
  }

  private def dissectHeader(input: Blob): Piece = {
    // Quake II's struct dmdl_t.

    val builder = new MoleculeBuilder

    builder.addChild("Identification", dissectString(input.slice(0, 4)))
    builder.addChild("Version", dissectInt(input.slice(4, 8)))

    builder.addChild("Skin width", dissectInt(input.slice(8, 12)))
    builder.addChild("Skin height", dissectInt(input.slice(12, 16)))
    builder.addChild("Frame size", dissectInt(input.slice(16, 20)))

    builder.addChild("Number of skins", dissectInt(input.slice(20, 24)))
    builder.addChild("Number of vertices", dissectInt(input.slice(24, 28)))
    builder.addChild("Number of texture coordinates", dissectInt(input.slice(28, 32)))
    builder.addChild("Number of triangles", dissectInt(input.slice(32, 36)))
    builder.addChild("Number of OpenGL commands", dissectInt(input.slice(36, 40)))
    builder.addChild("Number of frames", dissectInt(input.slice(40, 44)))

    builder.addChild("Offset of skins", dissectInt(input.slice(44, 48)))
    builder.addChild("Offset of texture coordinates", dissectInt(input.slice(48, 52)))
    builder.addChild("Offset of triangles", dissectInt(input.slice(52, 56)))
    builder.addChild("Offset of frames", dissectInt(input.slice(56, 60)))
    builder.addChild("Offset of OpenGL commands", dissectInt(input.slice(60, 64)))
    builder.addChild("File size", dissectInt(input.slice(64, 68)))

    builder.build()
  }

  override def dissect(input: Blob, offset: Offset): Piece = {

    Molecule(Some("MD2"), Seq(NamedPiece("Header", dissectHeader(input))))
  }
}
