/*
  This file is part of Octetoscope.
  Copyright (C) 2013-2016 Octetoscope contributors (see /AUTHORS.txt)

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
import org.scalatest.Inside._
import org.scalatest.MustMatchers._
import ru.corrigendum.octetoscope.core.PrimitiveDissectors._

class PrimitiveDissectorsSuite extends FunSuite {
  import PrimitiveDissectorsSuite._

  test("opaque (unsized)") {
    val dc = DissectionContext(new ArrayBlob(Array[Byte](0, 0)))
    opaque.dissect(dc, InfoSize(2, 0)) mustBe Atom(InfoSize(), EmptyContents)
    opaque.dissect(dc.copy(softLimit = Bytes(1)), InfoSize(0, 3)) mustBe Atom(InfoSize(0, 5), EmptyContents)
    opaque.dissect(dc.copy(softLimit = InfoSize()), InfoSize()) mustBe Atom(InfoSize(), EmptyContents)
  }

  test("opaque (sized)") {
    val dc = DissectionContext(new ArrayBlob(Array[Byte](0, 0, 0)))
    opaque(InfoSize(1, 7)).dissect(dc, InfoSize(1, 1)) mustBe Atom(InfoSize(1, 7), EmptyContents)
    opaque(InfoSize()).dissect(dc, InfoSize()) mustBe Atom(InfoSize(), EmptyContents)
    opaque(InfoSize()).dissect(dc, Bytes(3)) mustBe Atom(InfoSize(), EmptyContents)

    an [IndexOutOfBoundsException] must be thrownBy opaque(InfoSize(2, 0)).dissect(dc, InfoSize(1, 1))
    an [IndexOutOfBoundsException] must be thrownBy opaque(InfoSize()).dissect(dc, InfoSize(3, 1))
  }

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

  test("uInt16L") {
    verify(uInt16L, "0", 0, 0, 0)
    verify(uInt16L, "1", 1, 1, 0)
    verify(uInt16L, "128", 128, -128, 0)
    verify(uInt16L, "256", 256, 0, 1)
    verify(uInt16L, "32768", 32768, 0, -128)
    verify(uInt16L, "32769", 32769, 1, -128)
    verify(uInt16L, "65535", 65535, -1, -1)
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

  test("uInt32L") {
    verify(uInt32L, "0", 0L, 0, 0, 0, 0)
    verify(uInt32L, "1", 1L, 1, 0, 0, 0)
    verify(uInt32L, "255", 255L, -1, 0, 0, 0)
    verify(uInt32L, "256", 256L, 0, 1, 0, 0)
    verify(uInt32L, "65280", 65280L, 0, -1, 0, 0)
    verify(uInt32L, "65536", 65536L, 0, 0, 1, 0)
    verify(uInt32L, "16711680", 16711680L, 0, 0, -1, 0)
    verify(uInt32L, "16777216", 16777216L, 0, 0, 0, 1)
    verify(uInt32L, "4278190080", 4278190080L, 0, 0, 0, -1)
    verify(uInt32L, "4294967295", 4294967295L, -1, -1, -1, -1)
  }

  test("float32L") {
    verifyGeneric[Float](float32L, Some("-NaN(0x400001)"), num => Float.box(num) mustBe 'NaN,
      Seq.empty, 0x01, 0x00, 0xC0.toByte, 0xFF.toByte)
    verifyGeneric[Float](float32L, Some("-NaN(0x000001)"), num => Float.box(num) mustBe 'NaN,
      Seq.empty, 0x01, 0x00, 0x80.toByte, 0xFF.toByte)
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
      Seq.empty, 0x01, 0x00, 0x80.toByte, 0x7F.toByte)
    verifyGeneric[Float](float32L, Some("NaN(0x400001)"), num => Float.box(num) mustBe 'NaN,
      Seq.empty, 0x01, 0x00, 0xC0.toByte, 0x7F.toByte)
  }

  test("ascii(ish)String") {
    for ((dissector, severities) <- Seq((asciiString _, Seq(NoteSeverity.Failure)), (asciiishString _, Seq()))) {
      verify(dissector(0), "\"\"", Some(""))
      verify(dissector(4), "\"abcd\"", Some("abcd"), 'a', 'b', 'c', 'd')
      verify(dissector(999), "\"" + "abc" * 333 + "\"", Some("abc" * 333),
        Seq.fill(333)(Seq('a'.toByte, 'b'.toByte, 'c'.toByte)).flatten: _*)
      verify(dissector(34),
        "NUL SOH STX ETX EOT ENQ ACK BEL " +
          "BS HT LF VT FF CR SO SI " +
          "DLE DC1 DC2 DC3 DC4 NAK SYN ETB " +
          "CAN EM SUB ESC FS GS RS US " +
          "DEL QUOTE",
        Some(new String(('\u0000' to '\u001f').toArray :+ '\u007f' :+ '"')),
        ((0 to 0x1F) :+ 0x7F :+ '"'.toInt).map(_.toByte): _*)

      verifyWithSeverities(dissector(2), "0xf1f2", None, severities, 0xf1.toByte, 0xf2.toByte)
      verifyWithSeverities(dissector(5), "\"a\" 0xf1f2 \"bc\"", None, severities,
        'a', 0xf1.toByte, 0xf2.toByte, 'b', 'c')
      verifyWithSeverities(dissector(5), "\"1\" CR LF \"2\" 0xff", None, severities,
        '1', '\r', '\n', '2', 0xff.toByte)
    }
  }

  test("ascii(ish)ZString") {
    for ((dissector, severitiesOnDecodingErrors) <- Seq(
        (asciiZString _, Seq(NoteSeverity.Error, NoteSeverity.Failure)), (asciiishZString _, Seq(NoteSeverity.Error)))) {
      verify(dissector(4), "\"abc\"", Some("abc"), 'a', 'b', 'c', 0)
      verify(dissector(4), "\"ab\"", Some("ab"), 'a', 'b', 0, 'd')
      verifyWithSeverities(dissector(0), "\"\"", Some(""), Seq(NoteSeverity.Error))
      verifyWithSeverities(dissector(4), "\"abcd\"", Some("abcd"), Seq(NoteSeverity.Error), 'a', 'b', 'c', 'd')
      verifyWithSeverities(dissector(3), "\"a\" 0xf0 \"b\"", None, severitiesOnDecodingErrors, 'a', 0xf0.toByte, 'b')
    }
  }

  test("magic") {
    val dissector = magic(Array[Byte](1, 2, 3), "123")
    verify(dissector, "123", Some(()), 1, 2, 3)

    dissector.dissect(DissectionContext(new ArrayBlob(Array[Byte](4, 5, 6)))) mustBe
      Atom(Bytes(3), new EagerContents(None), Seq(Note(NoteSeverity.Failure, "expected \"123\" (0x010203)")))
  }

  test("bit") {
    val dc = DissectionContext(new ArrayBlob(Array[Byte](0x40, 0x7B)))
    bit.dissect(dc, InfoSize(0, 1)) mustBe Atom(Bits(1), new EagerContentsR(true, "True"))
    bit.dissect(dc, InfoSize(1, 5)) mustBe Atom(Bits(1), new EagerContentsR(false, "False"))
  }
}

object PrimitiveDissectorsSuite {
  def verifyGeneric[Value](
    dissector: DissectorC[Value], expectedRepr: Option[String], valueAssert: Value => Unit,
    expectedNoteSeverities: Seq[NoteSeverity.Value], bytes: Byte*
  ): Unit = {
    for (padSize <- List(0, 1)) {
      val pad = List.fill(padSize)(-1: Byte)
      val paddedBytes = pad ++ bytes ++ pad
      val blob = new ArrayBlob(paddedBytes.toArray)
      val piece = dissector.dissect(DissectionContext(blob), Bytes(padSize))

      inside(piece) { case Atom(size_, contents, notes) =>
        size_ mustBe Bytes(bytes.size)
        valueAssert(contents.value)
        contents.reprO mustBe expectedRepr
        notes.map(_.severity).sorted mustBe expectedNoteSeverities.sorted
      }
    }
  }

  def verify[Value](dissector: DissectorC[Value], expectedRepr: String, expectedValue: Value, bytes: Byte*): Unit = {
    verifyGeneric[Value](dissector, Some(expectedRepr), _ mustBe expectedValue, Seq.empty, bytes: _*)
  }

  def verifyWithSeverities[Value](
    dissector: DissectorC[Value], expectedRepr: String, expectedValue: Value,
    expectedNoteSeverities: Seq[NoteSeverity.Value], bytes: Byte*
  ): Unit = {
    verifyGeneric[Value](dissector, Some(expectedRepr), _ mustBe expectedValue, expectedNoteSeverities, bytes: _*)
  }
}
