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

class InfoSizeSuite extends FunSuite {
  test("totalBits") {
    Bytes(0).totalBits mustBe 0
    Bytes(3).totalBits mustBe 24
  }

  test("plus") {
    (Bytes(4) + InfoSize()) mustBe Bytes(4)
    (Bytes(4) + Bytes(1)) mustBe Bytes(5)
  }

  test("minus") {
    (Bytes(4) - Bytes(4)) mustBe InfoSize()
    (Bytes(8) - Bytes(4)) mustBe Bytes(4)
  }

  test("compare") {
    Bytes(5) must be < Bytes(7)
    Bytes(5) must be > Bytes(3)
    Bytes(5).compareTo(Bytes(5)) mustBe 0
  }

  test("equals") {
    Bytes(3).equals(null) mustBe false
    Bytes(3).equals(Bytes(1)) mustBe false
    Bytes(3).equals(Bytes(3)) mustBe true
  }

  test("toString") {
    Bytes(9).toString mustBe "Bytes(9)"
  }
}
