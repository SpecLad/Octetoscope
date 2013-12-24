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
import org.scalatest.matchers.ShouldMatchers._
import PrimitiveDissectors._

class PrimitiveDissectorsSuite extends FunSuite {
  import PrimitiveDissectorsSuite._

  test("SInt32L") {
    verify(sInt32L, "0", 0, 0, 0, 0, 0)
    verify(sInt32L, "67305985", 67305985, 1, 2, 3, 4)
    verify(sInt32L, "83754751", 83754751, -1, -2, -3, 4)
    verify(sInt32L, "-50462977", -50462977, -1, -2, -3, -4)
    verify(sInt32L, "-1", -1, -1, -1, -1, -1)

    verifyWithPad(sInt32L, "67305985", 67305985, 1, 2, 3, 4)
  }

  test("AsciiString") {
    verify(asciiString(4), "\"abcd\"", "abcd", 'a'.toByte, 'b'.toByte, 'c'.toByte, 'd'.toByte)
    verifyWithPad(asciiString(3), "\"efg\"", "efg", 'e'.toByte, 'f'.toByte, 'g'.toByte)
  }

  test("AsciiZString") {
    verify(asciiZString(4), "\"abcd\"", "abcd", 'a'.toByte, 'b'.toByte, 'c'.toByte, 'd'.toByte)
    verify(asciiZString(4), "\"abc\"", "abc", 'a'.toByte, 'b'.toByte, 'c'.toByte, 0)
    verify(asciiZString(4), "\"ab\"", "ab", 'a'.toByte, 'b'.toByte, 0, 'd'.toByte)
    verifyWithPad(asciiZString(3), "\"ef\"", "ef", 'e'.toByte, 'f'.toByte, 0)
  }

  test("Magic") {
    val dissector = magic(Array[Byte](1, 2, 3), "123")
    verify(dissector, "123", (), 1, 2, 3)
    verifyWithPad(dissector, "123", (), 1, 2, 3)

    dissector.dissectO(new ArrayBlob(Array[Byte](4, 5, 6))) should equal (
      Atom(Bytes(3), None, PieceQuality.Broken, Seq("expected \"123\" (0x010203)")),
      None
    )
  }
}

object PrimitiveDissectorsSuite {
  def verify[Value](dissector: DissectorO[Value], expectedRepr: String, expectedValue: Value, bytes: Byte*) {
    dissector.dissectO(new ArrayBlob(bytes.toArray)) should equal (
      (
        Atom(Bytes(bytes.size), Some(expectedRepr)),
        Some(expectedValue)
      )
    )
  }

  def verifyWithPad[Value](dissector: DissectorO[Value], expectedRepr: String, expectedValue: Value, bytes: Byte*) {
    val paddedBytes = (-1).toByte +: bytes :+ (-1).toByte
    val blob = new ArrayBlob(paddedBytes.toArray)
    dissector.dissectO(blob, Bytes(1)) should equal (
      (
        Atom(Bytes(bytes.size), Some(expectedRepr)),
        Some(expectedValue)
      )
    )
  }
}
