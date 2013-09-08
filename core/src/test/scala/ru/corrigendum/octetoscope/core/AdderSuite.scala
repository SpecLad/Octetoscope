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

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers._
import ru.corrigendum.octetoscope.core.PrimitiveDissectors._

class AdderSuite extends FunSuite {
  test("sequential adder") {
    val blob = new ArrayBlob(Array[Byte](-1, 1, 0, 0, 0, 2, 0, 0, 0, -1))
    val builder = new MoleculeBuilder

    val adder = new SequentialAdder(blob, Bytes(1), builder)
    adder("alpha", sInt32L) must equal (1)
    adder("beta", sInt32L) must equal (2)

    builder.build() must equal (Molecule(Bytes(8), None, Seq(
      SubPiece("alpha", Bytes(0), Atom(Bytes(4), Some("1"))),
      SubPiece("beta", Bytes(4), Atom(Bytes(4), Some("2")))
    )))
  }

  test("random adder") {
    val blob = new ArrayBlob(Array[Byte](-1, 3, 0, 0, 0, -1, 4, 0, 0, 0, -1))
    val builder = new MoleculeBuilder

    val adder = new RandomAdder(blob, Bytes(1), builder)
    adder("alpha", Bytes(0), sInt32L) must equal (3)
    adder("beta", Bytes(5), sInt32L) must equal (4)

    builder.build() must equal (Molecule(Bytes(9), None, Seq(
      SubPiece("alpha", Bytes(0), Atom(Bytes(4), Some("3"))),
      SubPiece("beta", Bytes(5), Atom(Bytes(4), Some("4")))
    )))
  }
}
