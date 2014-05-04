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
import org.scalatest.LoneElement._
import java.nio.charset.StandardCharsets
import PrimitiveDissectors._
import CompoundDissectors._

class CompoundDissectorsSuite extends FunSuite {
  def arrayTest[C <: Contents[Any]](dissector: Dissector[Any, C], contents: C) {
    val blob = new ArrayBlob("afoobarbazb".getBytes(StandardCharsets.US_ASCII))

    dissector.dissect(blob, Bytes(1)) mustBe
      Molecule(Bytes(9), contents, Seq(
        SubPiece("Item #0", Bytes(0), Atom(Bytes(3), new EagerContents("foo", Some("\"foo\"")))),
        SubPiece("Item #1", Bytes(3), Atom(Bytes(3), new EagerContents("bar", Some("\"bar\"")))),
        SubPiece("Item #2", Bytes(6), Atom(Bytes(3), new EagerContents("baz", Some("\"baz\""))))
      ))
  }

  test("array") {
    arrayTest(array(3, "Item", asciiString(3)), EmptyContents)
  }

  test("collectingArray") {
    arrayTest(collectingArray(3, "Item", asciiString(3)), new EagerContents(Seq("foo", "bar", "baz"), None))
  }

  test("collectingArray - with repr func") {
    arrayTest(collectingArray(3, "Item", asciiString(3), (seq: Seq[Any]) => seq.mkString(" - ")),
      new EagerContents(Seq("foo", "bar", "baz"), Some("foo - bar - baz")))
  }

  test("enum") {
    val blob = new ArrayBlob(Array[Byte](1, 2))
    val foo = new Object {
      override def toString: String = "FOO"
    }

    val dissector = enum(sInt8, Map(1.toByte -> foo))
    dissector.dissect(blob, Bytes(0)) mustBe Atom(Bytes(1), new EagerContents(Some(foo), Some("1 -> FOO")))

    val unknown = dissector.dissect(blob, Bytes(1))
    unknown.size mustBe Bytes(1)
    unknown.contents mustBe new EagerContents(None, Some("2"))
    unknown.notes.loneElement.pieceQuality mustBe Quality.Broken
  }
}
