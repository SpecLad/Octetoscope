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

    val adder = new SequentialAdder(blob, Offset(1), builder)
    adder("alpha", SInt32L) must equal (1)
    adder("beta", SInt32L) must equal (2)

    builder.build() must equal (Molecule(64, None, Seq(
      SubPiece("alpha", Offset(0), Atom(32, Some("1"))),
      SubPiece("beta", Offset(4), Atom(32, Some("2")))
    )))
  }

  test("random adder") {
    val blob = new ArrayBlob(Array[Byte](-1, 3, 0, 0, 0, -1, 4, 0, 0, 0, -1))
    val builder = new MoleculeBuilder

    val adder = new RandomAdder(blob, Offset(1), builder)
    adder("alpha", Offset(0), SInt32L) must equal (3)
    adder("beta", Offset(5), SInt32L) must equal (4)

    builder.build() must equal (Molecule(72, None, Seq(
      SubPiece("alpha", Offset(0), Atom(32, Some("3"))),
      SubPiece("beta", Offset(5), Atom(32, Some("4")))
    )))
  }
}
