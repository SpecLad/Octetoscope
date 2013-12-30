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
    CommonConstraints.nonNegative[Double].check(5.0) should equal (true)
    CommonConstraints.nonNegative[Int].check(0) should equal (true)
    CommonConstraints.nonNegative[Long].check(-1L) should equal (false)
  }

  test("positive") {
    CommonConstraints.positive[Short].check(4) should equal (true)
    CommonConstraints.positive[Byte].check(0) should equal (false)
    CommonConstraints.positive[Float].check(-2.0f) should equal (false)
  }
}
