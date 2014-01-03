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
import org.scalatest.Matchers._

class CommonConstraintsSuite extends FunSuite {
  test("nonNegative") {
    CommonConstraints.nonNegative[Double].check(5.0) shouldBe true
    CommonConstraints.nonNegative[Int].check(0) shouldBe true
    CommonConstraints.nonNegative[Long].check(-1L) shouldBe false
  }

  test("positive") {
    CommonConstraints.positive[Short].check(4) shouldBe true
    CommonConstraints.positive[Byte].check(0) shouldBe false
    CommonConstraints.positive[Float].check(-2.0f) shouldBe false
  }

  test("equalTo") {
    val c = CommonConstraints.equalTo(5, "FIVE")
    c.check(5) shouldBe true
    c.check(3) shouldBe false
    c.shouldNote should (include ("5") and include ("FIVE"))
    c.mustNote should (include ("5") and include ("FIVE"))
  }

  test("noMoreThan") {
    val c = CommonConstraints.noMoreThan(-2, "MINUS TWO")
    c.check(-5) shouldBe true
    c.check(-2) shouldBe true
    c.check(0) shouldBe false
    c.shouldNote should (include ("-2") and include ("MINUS TWO"))
    c.mustNote should (include ("-2") and include ("MINUS TWO"))
  }
}
