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

class PieceSuite extends FunSuite {
  test("impaired") {
    Atom(InfoSize(), None, PieceQuality.Dubious).impaired(PieceQuality.Bad).quality mustBe PieceQuality.Bad
    Atom(InfoSize(), None, PieceQuality.Bad).impaired(PieceQuality.Dubious).quality mustBe PieceQuality.Bad
    Molecule(InfoSize(), None, Seq(), PieceQuality.Dubious)
      .impaired(PieceQuality.Bad).quality mustBe PieceQuality.Bad
    Molecule(InfoSize(), None, Seq(), PieceQuality.Bad)
      .impaired(PieceQuality.Dubious).quality mustBe PieceQuality.Bad
  }

  test("withNote") {
    Atom(InfoSize(), None, notes = Seq("foo")).withNote("bar").notes mustBe Seq("foo", "bar")
    Molecule(InfoSize(), None, Seq(), notes = Seq("foo")).withNote("bar").notes mustBe Seq("foo", "bar")
  }
}
