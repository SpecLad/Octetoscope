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
import org.scalatest.Inside._
import org.scalatest.LoneElement._
import PrimitiveDissectors._

class PrimitiveDissectorsSuite extends FunSuite {
  import PrimitiveDissectorsSuite._

  test("sInt8") {
    verify(sInt8, "-128", -128, -128)
    verify(sInt8, "-100", -100, -100)
    verify(sInt8, "-1", -1, -1)
    verify(sInt8, "0", 0, 0)
    verify(sInt8, "1", 1, 1)
    verify(sInt8, "127", 127, 127)
  }

  test("uInt8") {
    verify(uInt8, "0", 0, 0)
    verify(uInt8, "1", 1, 1)
    verify(uInt8, "127", 127, 127)
    verify(uInt8, "128", 128, -128)
    verify(uInt8, "255", 255, -1)
  }

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

  test("float32L") {
    verifyGeneric[Float](float32L, Some("-NaN(0x400001)"), num => Float.box(num) mustBe 'NaN,
      None, 0x01, 0x00, 0xC0.toByte, 0xFF.toByte)
    verifyGeneric[Float](float32L, Some("-NaN(0x000001)"), num => Float.box(num) mustBe 'NaN,
      None, 0x01, 0x00, 0x80.toByte, 0xFF.toByte)
    verify(float32L, "-Infinity", Float.NegativeInfinity, 0x00, 0x00, 0x80.toByte, 0xFF.toByte)
    verify(float32L, "-1.50000000", -1.5f, 0x00, 0x00, 0xC0.toByte, 0xBF.toByte)
    verify(float32L, "-1.00000000", -1.0f, 0x00, 0x00, 0x80.toByte, 0xBF.toByte)
    verify(float32L, "-1.72477726e-34", -1.72477726e-34f, 0x21, 0x43, 0x65, 0x87.toByte)
    verify(float32L, "-8.83921135e-39", -8.83921135e-39f, 0x20, 0x40, 0x60, 0x80.toByte) /* subnormal */
    verify(float32L, "-0.00000000", -0.0f, 0x00, 0x00, 0x00, 0x80.toByte)
    verify(float32L, "0.00000000", 0.0f, 0x00, 0x00, 0x00, 0x00)
    verify(float32L, "1.48091464e-39", 1.48091464e-39f, 0x30, 0x20, 0x10, 0x00) /* subnormal */
    verify(float32L, "5.00000000", 5.0f, 0x00, 0x00, 0xA0.toByte, 0x40)
    verify(float32L, "5.50000000", 5.5f, 0x00, 0x00, 0xB0.toByte, 0x40)
    verify(float32L, "1.73782444e+34", 1.73782444e+34f, 0x12, 0x34, 0x56, 0x78)
    verify(float32L, "Infinity", Float.PositiveInfinity, 0x00, 0x00, 0x80.toByte, 0x7F)
    verifyGeneric[Float](float32L, Some("NaN(0x000001)"), num => Float.box(num) mustBe 'NaN,
      None, 0x01, 0x00, 0x80.toByte, 0x7F.toByte)
    verifyGeneric[Float](float32L, Some("NaN(0x400001)"), num => Float.box(num) mustBe 'NaN,
      None, 0x01, 0x00, 0xC0.toByte, 0x7F.toByte)
  }

  test("asciiString") {
    verify(asciiString(0), "\"\"", Some(""))
    verify(asciiString(4), "\"abcd\"", Some("abcd"), 'a', 'b', 'c', 'd')
  }

  test("asciiZString") {
    verify(asciiZString(4), "\"abc\"", Some("abc"), 'a', 'b', 'c', 0)
    verify(asciiZString(4), "\"ab\"", Some("ab"), 'a', 'b', 0, 'd')
    verifyBad(asciiZString(0), "\"\"", Some(""))
    verifyBad(asciiZString(4), "\"abcd\"", Some("abcd"), 'a', 'b', 'c', 'd')
  }

  test("magic") {
    val dissector = magic(Array[Byte](1, 2, 3), "123")
    verify(dissector, "123", Some(()), 1, 2, 3)

    dissector.dissect(new ArrayBlob(Array[Byte](4, 5, 6))) mustBe
      Atom(Bytes(3), new EagerContents(None), Seq(Note(Quality.Broken, "expected \"123\" (0x010203)")))
  }

  test("bit") {
    val blob = new ArrayBlob(Array[Byte](0x40, 0x7B))
    bit.dissect(blob, InfoSize(0, 1)) mustBe Atom(Bits(1), new EagerContentsR(true, "True"))
    bit.dissect(blob, InfoSize(1, 5)) mustBe Atom(Bits(1), new EagerContentsR(false, "False"))
  }
}

object PrimitiveDissectorsSuite {
  def verifyGeneric[Value](
    dissector: DissectorC[Value], expectedRepr: Option[String], valueAssert: Value => Unit,
    expectedNoteQuality: Option[Quality.Value], bytes: Byte*
  ) {
    for (padSize <- List(0, 1)) {
      val pad = List.fill(padSize)((-1).toByte)
      val paddedBytes = pad ++ bytes ++ pad
      val blob = new ArrayBlob(paddedBytes.toArray)
      val piece = dissector.dissect(blob, Bytes(padSize))

      inside(piece) { case Atom(size_, contents, notes) =>
        size_ mustBe Bytes(bytes.size)
        valueAssert(contents.value)
        contents.reprO mustBe expectedRepr
        expectedNoteQuality.foreach(q => notes.loneElement.pieceQuality mustBe q)
      }
    }
  }

  def verify[Value](dissector: DissectorC[Value], expectedRepr: String, expectedValue: Value, bytes: Byte*) {
    verifyGeneric[Value](dissector, Some(expectedRepr), _ mustBe expectedValue, None, bytes: _*)
  }

  def verifyBad[Value](dissector: DissectorC[Value], expectedRepr: String, expectedValue: Value, bytes: Byte*) {
    verifyGeneric[Value](dissector, Some(expectedRepr), _ mustBe expectedValue, Some(Quality.Bad), bytes: _*)
  }
}
