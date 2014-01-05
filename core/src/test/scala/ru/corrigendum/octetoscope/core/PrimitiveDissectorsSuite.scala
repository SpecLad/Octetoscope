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
import org.scalatest.Inside._
import PrimitiveDissectors._

class PrimitiveDissectorsSuite extends FunSuite {
  import PrimitiveDissectorsSuite._

  test("sInt16L") {
    verify(sInt16L, "-32768", -32768, 0, -128)
    verify(sInt16L, "-257", -257, -1, -2)
    verify(sInt16L, "-1", -1, -1, -1)
    verify(sInt16L, "0", 0, 0, 0)
    verify(sInt16L, "513", 513, 1, 2)
    verify(sInt16L, "767", 767, -1, 2)
    verify(sInt16L, "32767", 32767, -1, 127)
  }

  test("sInt32L") {
    verify(sInt32L, "-2147483648", -2147483648, 0, 0, 0, -128)
    verify(sInt32L, "-50462977", -50462977, -1, -2, -3, -4)
    verify(sInt32L, "-1", -1, -1, -1, -1, -1)
    verify(sInt32L, "0", 0, 0, 0, 0, 0)
    verify(sInt32L, "67305985", 67305985, 1, 2, 3, 4)
    verify(sInt32L, "83754751", 83754751, -1, -2, -3, 4)
    verify(sInt32L, "2147483647", 2147483647, -1, -1, -1, 127)
  }

  test("asciiString") {
    verify(asciiString(0), "\"\"", "")
    verify(asciiString(4), "\"abcd\"", "abcd", 'a', 'b', 'c', 'd')
  }

  test("asciiZString") {
    verify(asciiZString(4), "\"abc\"", "abc", 'a', 'b', 'c', 0)
    verify(asciiZString(4), "\"ab\"", "ab", 'a', 'b', 0, 'd')
    verifyBad(asciiZString(0), "\"\"", "")
    verifyBad(asciiZString(4), "\"abcd\"", "abcd", 'a', 'b', 'c', 'd')
  }

  test("magic") {
    val dissector = magic(Array[Byte](1, 2, 3), "123")
    verify(dissector, "123", (), 1, 2, 3)

    dissector.dissectO(new ArrayBlob(Array[Byte](4, 5, 6))) shouldBe (
      Atom(Bytes(3), None, PieceQuality.Broken, Seq("expected \"123\" (0x010203)")),
      None
    )
  }
}

object PrimitiveDissectorsSuite {
  def verifyGeneric[Value](
    dissector: DissectorO[Value], expectedRepr: Option[String], expectedValue: Option[Value],
    expectedQuality: PieceQuality.Value, expectedNumNotes: Int, bytes: Byte*
  ) {
    for (padSize <- List(0, 1)) {
      val pad = List.fill(padSize)((-1).toByte)
      val paddedBytes = pad ++ bytes ++ pad
      val blob = new ArrayBlob(paddedBytes.toArray)
      val (piece, value) = dissector.dissectO(blob, Bytes(padSize))

      inside(piece) { case Atom(size_, repr, quality, notes) =>
        size_ shouldBe Bytes(bytes.size)
        repr shouldBe expectedRepr
        quality shouldBe expectedQuality
        notes should have size expectedNumNotes
      }

      value shouldBe expectedValue
    }
  }

  def verify[Value](dissector: DissectorO[Value], expectedRepr: String, expectedValue: Value, bytes: Byte*) {
    verifyGeneric(dissector, Some(expectedRepr), Some(expectedValue), PieceQuality.Good, 0, bytes: _*)
  }

  def verifyBad[Value](dissector: DissectorO[Value], expectedRepr: String, expectedValue: Value, bytes: Byte*) {
    verifyGeneric(dissector, Some(expectedRepr), Some(expectedValue), PieceQuality.Bad, 1, bytes: _*)
  }
}
