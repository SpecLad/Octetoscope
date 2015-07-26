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

import org.scalatest.MustMatchers._
import org.scalatest.{FunSuite, OneInstancePerTest}
import ru.corrigendum.octetoscope.abstractinfra.Blob

class ArrayBlobSuite extends FunSuite with OneInstancePerTest {
  private[this] val blob: Blob = new ArrayBlob(Array[Byte](3, 4, 5, 6, 7))

  test("apply - normal") {
    blob(1) mustBe 4
  }

  test("apply - out of range") {
    an [IndexOutOfBoundsException] must be thrownBy { blob(5) }
    an [IndexOutOfBoundsException] must be thrownBy { blob(-1) }
  }

  test("size") {
    blob.size mustBe 5
  }

  test("getRangeAsArray - normal") {
    blob.getRangeAsArray(2, 4) mustBe Array[Byte](5, 6)
    blob.getRangeAsArray(0, 4) mustBe Array[Byte](3, 4, 5, 6)
    blob.getRangeAsArray(2, 5) mustBe Array[Byte](5, 6, 7)
    blob.getRangeAsArray(0, 5) mustBe Array[Byte](3, 4, 5, 6, 7)
    blob.getRangeAsArray(2, 2) mustBe Array[Byte]()
  }

  test("getRangeAsArray - out of range") {
    an [IndexOutOfBoundsException] must be thrownBy { blob.getRangeAsArray(-1, 4) }
    an [IndexOutOfBoundsException] must be thrownBy { blob.getRangeAsArray(5, 6) }
    an [IndexOutOfBoundsException] must be thrownBy { blob.getRangeAsArray(2, 6) }
    an [IndexOutOfBoundsException] must be thrownBy { blob.getRangeAsArray(-2, -1) }
    an [IndexOutOfBoundsException] must be thrownBy { blob.getRangeAsArray(4, 2) }
  }
}
