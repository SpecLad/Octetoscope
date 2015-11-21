/*
  This file is part of Octetoscope.
  Copyright (C) 2013-2015 Octetoscope contributors (see /AUTHORS.txt)

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
import ru.corrigendum.octetoscope.core.mocks.{MockConstraint, MockDissector}

class SpecialDissectorsSuite extends FunSuite {
  test("transformed") {
    val note = Note(NoteSeverity.Info, "transformed")
    val transform = (piece: PieceCR[String]) => piece.withNote(note)

    val transformed = SpecialDissectors.transformed(MockDissector, transform)

    val blob = new ArrayBlob(Array[Byte]('0'))

    val piece = transformed.dissect(DissectionContext(blob))

    piece mustBe Atom(Bytes(1), new ToStringContents("0"), notes = Seq(note))
  }

  test("constrained - satisfied") {
    val constrained = SpecialDissectors.constrained(MockDissector, MockConstraint, NoteSeverity.Warning)

    val dc = DissectionContext(new ArrayBlob(Array[Byte]('a', 'b')))

    constrained.dissect(dc) mustBe MockDissector.dissect(dc)
  }

  test("constrained - unsatisfied") {
    val constrained = SpecialDissectors.constrained(MockDissector, MockConstraint, NoteSeverity.Warning)

    val blob = new ArrayBlob(Array[Byte]('a'))

    val piece = constrained.dissect(DissectionContext(blob))

    piece mustBe Atom(Bytes(1), new ToStringContents("a"), Seq(Note(NoteSeverity.Warning, "constrained (Warning)")))
  }
}
