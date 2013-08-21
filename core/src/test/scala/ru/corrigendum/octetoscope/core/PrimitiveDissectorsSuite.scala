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
import PrimitiveDissectors._

class PrimitiveDissectorsSuite extends FunSuite {
  import PrimitiveDissectorsSuite._

  test("SInt32L") {
    verify(SInt32L, "0", 0, 0, 0, 0)
    verify(SInt32L, "67305985", 1, 2, 3, 4)
    verify(SInt32L, "83754751", -1, -2, -3, 4)
    verify(SInt32L, "-50462977", -1, -2, -3, -4)
    verify(SInt32L, "-1", -1, -1, -1, -1)

    verifyWithPad(SInt32L, "67305985", 1, 2, 3, 4)
  }
}

object PrimitiveDissectorsSuite {
  def verify(dissector: Dissector, expectedRepr: String, bytes: Byte*) {
    dissector.dissect(new ArrayBlob(bytes.toArray)) must equal (Atom(Some(expectedRepr)))
  }

  def verifyWithPad(dissector: Dissector, expectedRepr: String, bytes: Byte*) {
    val paddedBytes = (-1).toByte +: bytes :+ (-1).toByte
    val blob = new ArrayBlob(paddedBytes.toArray)
    dissector.dissect(blob, Offset(1)) must equal (Atom(Some(expectedRepr)))
  }
}
