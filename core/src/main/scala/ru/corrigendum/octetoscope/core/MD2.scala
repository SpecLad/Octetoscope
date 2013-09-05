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

object MD2 extends MoleculeBuilderDissector[Unit] {
  private object Header extends MoleculeBuilderDissector[HeaderValue] {
    // Quake II's struct dmdl_t.
    override def dissect(input: Blob, offset: InfoSize, builder: MoleculeBuilder) = {
      val adder = new SequentialAdder(input, offset, builder)
      val value = new HeaderValue

      adder("Identification", asciiString(4))
      adder("Version", sInt32L)

      adder("Skin width", sInt32L)
      adder("Skin height", sInt32L)
      adder("Frame size", sInt32L)

      value.numSkins = adder("Number of skins", sInt32L)
      adder("Number of vertices", sInt32L)
      adder("Number of texture coordinates", sInt32L)
      adder("Number of triangles", sInt32L)
      adder("Number of OpenGL commands", sInt32L)
      adder("Number of frames", sInt32L)

      value.offSkins = adder("Offset of skins", sInt32L)
      adder("Offset of texture coordinates", sInt32L)
      adder("Offset of triangles", sInt32L)
      adder("Offset of frames", sInt32L)
      adder("Offset of OpenGL commands", sInt32L)
      adder("File size", sInt32L)

      value
    }
  }

  private class HeaderValue {
    var numSkins: Int = _
    var offSkins: Int = _
  }

  private class Skins(numSkins: Int) extends MoleculeBuilderDissector[Unit] {
    override def dissect(input: Blob, offset: InfoSize, builder: MoleculeBuilder) {
      val adder = new SequentialAdder(input, offset, builder)

      for (_ <- 0 until numSkins)
        adder("Skin", asciiZString(64))
    }
  }

  override def dissect(input: Blob, offset: InfoSize, builder: MoleculeBuilder) {
    val adder = new RandomAdder(input, offset, builder)
    val header = adder("Header", Bytes(0), Header)
    adder("Skins", Bytes(header.offSkins), new Skins(header.numSkins))
    builder.setRepr("MD2")
  }
}
