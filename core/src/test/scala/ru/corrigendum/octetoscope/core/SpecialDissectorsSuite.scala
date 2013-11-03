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
import org.scalatest.matchers.ShouldMatchers._
import ru.corrigendum.octetoscope.core.mocks.{MockConstraint, MockDissector}
import ru.corrigendum.octetoscope.core.SpecialDissectors.Constraint

class SpecialDissectorsSuite extends FunSuite {
  test("transformed") {
    val transform = (piece: Piece, value: String) => (piece.withNote("transformed"), Some(value + "!"))

    val transformed = SpecialDissectors.transformed(MockDissector, transform)

    val blob = new ArrayBlob(Array[Byte]('?'.toByte))

    val (piece, value) = transformed.dissect(blob)

    piece should equal (Atom(Bytes(1), Some("?"), notes = Seq("transformed")))
    value should equal (Some("?!"))
  }

  test("constrained - satisfied") {
    val constrained = SpecialDissectors.constrained(MockDissector, MockConstraint, PieceQuality.Dubious)

    val blob = new ArrayBlob(Array[Byte]('a'.toByte, 'b'.toByte))

    constrained.dissect(blob) should equal (MockDissector.dissect(blob))
  }

  test("constrained - unsatisfied") {
    val constrained = SpecialDissectors.constrained(MockDissector, MockConstraint, PieceQuality.Dubious)

    val blob = new ArrayBlob(Array[Byte]('a'.toByte))

    val (piece, value) = constrained.dissect(blob)

    piece should equal (Atom(Bytes(1), Some("a"), PieceQuality.Dubious, Seq("constrained")))
  }

  test("constrained - unsatisfied & already worse") {
    val constrained = SpecialDissectors.constrained(
      SpecialDissectors.constrained(MockDissector, MockConstraint, PieceQuality.Bad),
      MockConstraint, PieceQuality.Dubious
    )

    val blob = new ArrayBlob(Array[Byte]('a'.toByte))

    val (piece, value) = constrained.dissect(blob)

    piece should equal (Atom(Bytes(1), Some("a"), PieceQuality.Bad, Seq("constrained", "constrained")))
  }
}
