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

import java.nio.charset.StandardCharsets

import org.scalatest.FunSuite
import org.scalatest.LoneElement._
import org.scalatest.MustMatchers._
import ru.corrigendum.octetoscope.core.CompoundDissectors._
import ru.corrigendum.octetoscope.core.PrimitiveDissectors._

class CompoundDissectorsSuite extends FunSuite {
  def arrayTest[C <: Contents[Any]](dissector: Dissector[Any, C], contents: C): Unit = {
    val blob = new ArrayBlob("afoobarbazb".getBytes(StandardCharsets.US_ASCII))

    dissector.dissect(DissectionContext(blob), Bytes(1)) mustBe
      Molecule(Bytes(9), contents, Seq(
        SubPiece("Item #0", Bytes(0), Atom(Bytes(3), new EagerContentsR(Some("foo"), "\"foo\""))),
        SubPiece("Item #1", Bytes(3), Atom(Bytes(3), new EagerContentsR(Some("bar"), "\"bar\""))),
        SubPiece("Item #2", Bytes(6), Atom(Bytes(3), new EagerContentsR(Some("baz"), "\"baz\"")))
      ))
  }

  test("array") {
    arrayTest(array(3, "Item", asciiString(3)), EmptyContents)
  }

  test("collectingArray") {
    arrayTest(collectingArray(3, "Item", asciiString(3)),
      new EagerContents(Seq(Some("foo"), Some("bar"), Some("baz")), None))
  }

  test("collectingArray - with repr func") {
    arrayTest(collectingArray(3, "Item", asciiString(3), (seq: Seq[Option[Any]]) => seq.map(_.get).mkString(" - ")),
      new EagerContents(Seq(Some("foo"), Some("bar"), Some("baz")), Some("foo - bar - baz")))
  }

  test("enum") {
    val blob = new ArrayBlob(Array[Byte](1, 2))
    val foo = new Object {
      override def toString: String = "FOO"
    }

    val dissector = enum(sInt8, Map(1.toByte -> foo))
    dissector.dissect(DissectionContext(blob), Bytes(0)) mustBe
      Atom(Bytes(1), new EagerContentsR(Some(foo), "1 -> FOO"))

    val unknown = dissector.dissect(DissectionContext(blob), Bytes(1))
    unknown.size mustBe Bytes(1)
    unknown.contents mustBe new EagerContentsR(None, "2")
    unknown.notes.loneElement.severity mustBe NoteSeverity.Failure
  }

  private def bitFieldTest(byte: Byte, repr: String): Unit = {
    val blob = new ArrayBlob(Array[Byte](byte))
    val dc = DissectionContext(blob)
    val dissector = bitField(4, Map(1L -> "A", 2L -> "B"), unnamedReason = "xyzzy")
    val piece = dissector.dissect(dc, Bits(2))

    val value = (if ((byte & 0x10) != 0) Set("A") else Set()) ++ (if ((byte & 0x08) != 0) Set("B") else Set())

    piece mustBe Molecule(
      Bits(4),
      new EagerContentsR(value, repr),
      Seq(
        SubPiece("Bit #0 (xyzzy)", Bits(0), bit.dissect(dc, Bits(2))),
        SubPiece("A", Bits(1), bit.dissect(dc, Bits(3))),
        SubPiece("B", Bits(2), bit.dissect(dc, Bits(4))),
        SubPiece("Bit #3 (xyzzy)", Bits(3), bit.dissect(dc, Bits(5)))
      ),
      Nil)
  }

  test("bitField - no bits set") {
    bitFieldTest(0x42, "<>")
  }

  test("bitField - known bit set") {
    bitFieldTest(0x4a, "<B>")
  }

  test("bitField - unknown bit set") {
    bitFieldTest(0x62, "<#0>")
  }

  test("bitField - multiple bits set") {
    bitFieldTest(0x56, "<A | #3>")
  }

  test("bitField - should be zero") {
    val blob = new ArrayBlob(Array[Byte](0x01))
    val dc = DissectionContext(blob)
    val dissector = bitField(2, Map(0L -> "A", 1L -> "B"), sbz = Set("A", "B"))
    val piece = dissector.dissect(dc, Bits(6))

    piece.children(0).piece mustBe bit.dissect(dc, Bits(6))
    piece.children(1).piece mustBe (bit +? CommonConstraints.`false`).dissect(dc, Bits(7))
  }
}
