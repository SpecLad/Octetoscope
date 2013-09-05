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

class OffsetSuite extends FunSuite {
  test("totalBits") {
    Bytes(0).totalBits must equal (0)
    Bytes(3).totalBits must equal (24)
  }

  test("plus") {
    (Bytes(4) + 0) must equal (Bytes(4))
    (Bytes(4) + 8) must equal (Bytes(5))
    (Bytes(4) + -16) must equal (Bytes(2))
  }

  test("minus") {
    (Bytes(4) - Bytes(4)) must equal (0)
    (Bytes(8) - Bytes(4)) must equal (32)
    (Bytes(3) - Bytes(4)) must equal (-8)
  }
}
