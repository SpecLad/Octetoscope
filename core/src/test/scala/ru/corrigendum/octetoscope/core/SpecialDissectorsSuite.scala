/*
  This file is part of Octetoscope.
  Copyright (C) 2013-2014 Octetoscope contributors (see /AUTHORS.txt)

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
import org.scalatest.MustMatchers._
import ru.corrigendum.octetoscope.core.mocks.{MockDissectorO, MockConstraint, MockDissector}

class SpecialDissectorsSuite extends FunSuite {
  test("transformed") {
    val note = PieceNote(PieceQuality.Good, "transformed")
    val transform = (piece: Piece, value: String) => (piece.withNote(note), value.toInt)

    val transformed = SpecialDissectors.transformed(MockDissector, transform)

    val blob = new ArrayBlob(Array[Byte]('0'))

    val (piece, value) = transformed.dissect(blob)

    piece mustBe Atom(Bytes(1), Some("0"), notes = Seq(note))
    value mustBe 0
  }

  test("constrained - satisfied") {
    val constrained = SpecialDissectors.constrained(MockDissector, MockConstraint, PieceQuality.Dubious)

    val blob = new ArrayBlob(Array[Byte]('a', 'b'))

    constrained.dissect(blob) mustBe MockDissector.dissect(blob)
  }

  test("constrained - unsatisfied") {
    val constrained = SpecialDissectors.constrained(MockDissector, MockConstraint, PieceQuality.Dubious)

    val blob = new ArrayBlob(Array[Byte]('a'))

    val (piece, value) = constrained.dissect(blob)

    piece mustBe Atom(Bytes(1), Some("a"), Seq(PieceNote(PieceQuality.Dubious, "constrained (Dubious)")))
    value mustBe "a"
  }

  test("transformedO - None") {
    val transform = (piece: Piece, value: String) =>
      (piece.withNote(PieceNote(PieceQuality.Good, "transformed")), Some(value + "!"))

    val transformed = SpecialDissectors.transformedO(MockDissectorO, transform)

    transformed.dissectO(null) mustBe MockDissectorO.dissectO(null)
  }

  test("transformedO - Some") {
    val note = PieceNote(PieceQuality.Good, "transformed")

    val transform = (piece: Piece, value: String) => (piece.withNote(note), Some(value.toInt))

    val transformed = SpecialDissectors.transformedO(MockDissector, transform)

    val blob = new ArrayBlob(Array[Byte]('0'))

    val (piece, value) = transformed.dissectO(blob)

    piece mustBe Atom(Bytes(1), Some("0"), notes = Seq(note))
    value mustBe Some(0)
  }

  test("constrainedO - None") {
    val constrained = SpecialDissectors.constrainedO(MockDissectorO, MockConstraint, PieceQuality.Dubious)

    constrained.dissectO(null) mustBe MockDissectorO.dissectO(null)
  }

  test("constrainedO - Some - satisfied") {
    val constrained = SpecialDissectors.constrainedO(MockDissector, MockConstraint, PieceQuality.Dubious)

    val blob = new ArrayBlob(Array[Byte]('a', 'b'))

    constrained.dissectO(blob) mustBe MockDissector.dissectO(blob)
  }

  test("constrainedO - Some - unsatisfied") {
    val constrained = SpecialDissectors.constrainedO(MockDissector, MockConstraint, PieceQuality.Dubious)

    val blob = new ArrayBlob(Array[Byte]('a'))

    val (piece, value) = constrained.dissectO(blob)

    piece mustBe Atom(Bytes(1), Some("a"), Seq(PieceNote(PieceQuality.Dubious, "constrained (Dubious)")))
    value mustBe Some("a")
  }

  test("stronglyConstrainedO - None") {
    val constrained = SpecialDissectors.stronglyConstrainedO(MockDissectorO, MockConstraint, PieceQuality.Dubious)

    constrained.dissectO(null) mustBe MockDissectorO.dissectO(null)
  }

  test("stronglyConstrainedO - Some - satisfied") {
    val constrained = SpecialDissectors.stronglyConstrainedO(MockDissector, MockConstraint, PieceQuality.Dubious)

    val blob = new ArrayBlob(Array[Byte]('a', 'b'))

    constrained.dissectO(blob) mustBe MockDissector.dissectO(blob)
  }

  test("stronglyConstrainedO - Some - unsatisfied") {
    val constrained = SpecialDissectors.stronglyConstrainedO(MockDissector, MockConstraint, PieceQuality.Dubious)

    val blob = new ArrayBlob(Array[Byte]('a'))

    val (piece, value) = constrained.dissectO(blob)

    piece mustBe Atom(Bytes(1), Some("a"), Seq(PieceNote(PieceQuality.Dubious, "constrained (Dubious)")))
    value mustBe None
  }
}
