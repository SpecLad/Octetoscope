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

import org.scalatest.{BeforeAndAfter, FunSuite}
import org.scalatest.Matchers._
import java.util
import ru.corrigendum.octetoscope.abstractinfra.Blob

class ArrayBlobSuite extends FunSuite with BeforeAndAfter {
  var blob: Blob = _

  before {
    blob = new ArrayBlob(Array[Byte](1, 2, 3, 4, 5, 6, 7, 8, 9)).slice(2, 7)
  }

  test("apply - normal") {
    blob(1) shouldBe 4
  }

  test("apply - out of range") {
    evaluating { blob(5) } should produce [IndexOutOfBoundsException]
    evaluating { blob(-1) } should produce [IndexOutOfBoundsException]
  }

  test("toArray") {
    blob.toArray shouldBe Array[Byte](3, 4, 5, 6, 7)
  }

  test("size") {
    blob.size shouldBe 5
  }

  test("slice - normal") {
    blob.slice(2, 4).toArray shouldBe Array[Byte](5, 6)
    blob.slice(0, 4).toArray shouldBe Array[Byte](3, 4, 5, 6)
    blob.slice(2, 5).toArray shouldBe Array[Byte](5, 6, 7)
    blob.slice(0, 5).toArray shouldBe Array[Byte](3, 4, 5, 6, 7)
    blob.slice(2, 2).toArray shouldBe Array[Byte]()
  }

  test("slice - out of range") {
    evaluating { blob.slice(-1, 4) } should produce [IndexOutOfBoundsException]
    evaluating { blob.slice(5, 6) } should produce [IndexOutOfBoundsException]
    evaluating { blob.slice(2, 6) } should produce [IndexOutOfBoundsException]
    evaluating { blob.slice(-2, -1) } should produce [IndexOutOfBoundsException]
    evaluating { blob.slice(4, 2) } should produce [IndexOutOfBoundsException]
  }
}
