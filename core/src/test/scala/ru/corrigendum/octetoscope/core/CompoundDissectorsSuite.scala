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
  test("array") {
    val blob = new ArrayBlob("afoobarbazb".getBytes(StandardCharsets.US_ASCII))

    array(3, "Item", asciiString(3)).dissect(blob, Bytes(1)) shouldBe (
      Molecule(Bytes(9), None, Seq(
        SubPiece("Item #0", Bytes(0), Atom(Bytes(3), Some("\"foo\""))),
        SubPiece("Item #1", Bytes(3), Atom(Bytes(3), Some("\"bar\""))),
        SubPiece("Item #2", Bytes(6), Atom(Bytes(3), Some("\"baz\"")))
      )),
      ())
  }
}
