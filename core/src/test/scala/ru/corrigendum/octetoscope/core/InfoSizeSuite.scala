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
  test("factories") {
    InfoSize(3, 4) mustBe new InfoSize(3, 4)
    Bytes(3) mustBe new InfoSize(3, 0)
    Bits(3) mustBe new InfoSize(0, 3)
    Bits(28) mustBe new InfoSize(3, 4)
  }

  test("extractors") {
    InfoSize.unapply(InfoSize(3, 4)) mustBe Some((3, 4))
    Bytes.unapply(InfoSize(3, 4)) mustBe None
    Bytes.unapply(Bytes(3)) mustBe Some(3)
    Bits.unapply(InfoSize(3, 4)) mustBe Some(28)
  }

  test("totalBits") {
    InfoSize().totalBits mustBe 0
    Bytes(3).totalBits mustBe 24
    Bits(3).totalBits mustBe 3
    InfoSize(3, 3).totalBits mustBe 27
  }

  test("plus") {
    val augend = InfoSize(3, 4)
    (augend + InfoSize()) mustBe augend
    (augend + InfoSize(1, 3)) mustBe InfoSize(4, 7)
    (augend + InfoSize(1, 5)) mustBe InfoSize(5, 1)
  }

  test("minus") {
    val minuend = InfoSize(3, 4)
    (minuend - InfoSize()) mustBe minuend
    (minuend - InfoSize(1, 3)) mustBe InfoSize(2, 1)
    (minuend - InfoSize(1, 5)) mustBe InfoSize(1, 7)
  }

  test("compare") {
    InfoSize(3, 5) must be < InfoSize(4, 3)
    InfoSize(3, 5) must be < InfoSize(3, 7)
    InfoSize(3, 5).compare(InfoSize(3, 5)) mustBe 0
    InfoSize(3, 5) must be > InfoSize(3, 3)
    InfoSize(3, 5) must be > InfoSize(2, 7)
  }

  test("equals") {
    InfoSize(3, 5).equals(null) mustBe false
    InfoSize(3, 5).equals(InfoSize(3, 4)) mustBe false
    InfoSize(3, 5).equals(InfoSize(4, 5)) mustBe false
    InfoSize(3, 5).equals(InfoSize(3, 5)) mustBe true
  }

  test("toString") {
    InfoSize().toString mustBe "InfoSize()"
    InfoSize(9, 0).toString mustBe "Bytes(9)"
    InfoSize(0, 5).toString mustBe "Bits(5)"
    InfoSize(9, 5).toString mustBe "InfoSize(9, 5)"
  }
}
