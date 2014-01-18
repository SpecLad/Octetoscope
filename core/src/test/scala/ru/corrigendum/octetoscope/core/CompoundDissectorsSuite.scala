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
import java.nio.charset.StandardCharsets
import PrimitiveDissectors._
import CompoundDissectors._

class CompoundDissectorsSuite extends FunSuite {
  def arrayTest[T](dissector: Dissector[T], repr: Option[String], value: T) {
    val blob = new ArrayBlob("afoobarbazb".getBytes(StandardCharsets.US_ASCII))

    dissector.dissect(blob, Bytes(1)) shouldBe (
      Molecule(Bytes(9), repr, Seq(
        SubPiece("Item #0", Bytes(0), Atom(Bytes(3), Some("\"foo\""))),
        SubPiece("Item #1", Bytes(3), Atom(Bytes(3), Some("\"bar\""))),
        SubPiece("Item #2", Bytes(6), Atom(Bytes(3), Some("\"baz\"")))
      )),
      value)
  }

  test("array") {
    arrayTest(array(3, "Item", asciiString(3)), None, ())
  }

  test("collectingArray") {
    arrayTest(collectingArray(3, "Item", asciiString(3)), None, Seq("foo", "bar", "baz"))
  }

  test("collectingArray - with repr func") {
    arrayTest(collectingArray(3, "Item", asciiString(3), (seq: Seq[Any]) => seq.mkString(" - ")),
      Some("foo - bar - baz"), Seq("foo", "bar", "baz"))
  }
}
