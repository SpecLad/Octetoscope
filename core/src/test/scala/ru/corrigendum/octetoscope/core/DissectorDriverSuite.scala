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

import java.io.File
import java.nio.charset.StandardCharsets

import org.scalatest.FunSuite
import org.scalatest.MustMatchers._
import ru.corrigendum.octetoscope.abstractinfra.Blob
import ru.corrigendum.octetoscope.core.mocks.{MockBinaryReader, MockDissector}

class DissectorDriverSuite extends FunSuite {
  test("success") {
    val reader = new MockBinaryReader(new ArrayBlob("magic".getBytes(StandardCharsets.US_ASCII)))
    val driver = getDissectorDriver(reader, Function.const(Some(MockDissector)))
    driver(DissectorDriverSuite.FakePath) mustBe Atom(Bytes(5), new ToStringContents("magic"))
  }

  test("failure - file too small") {
    val reader = new MockBinaryReader(Blob.empty)
    val driver = getDissectorDriver(reader, Function.const(Some(new PlainDissector {
      override def dissect(input: Blob, offset: InfoSize): PlainPiece = throw new IndexOutOfBoundsException
    })))

    a [TooSmallToDissectException] must be thrownBy driver(DissectorDriverSuite.FakePath)
  }

  test("failure - detection failed") {
    val reader = new MockBinaryReader(Blob.empty)
    val driver = getDissectorDriver(reader, Function.const(None))

    a [DetectionFailedException] must be thrownBy driver(DissectorDriverSuite.FakePath)
  }
}

object DissectorDriverSuite {
  val FakePath = new File("/abra/cadabra")
}
