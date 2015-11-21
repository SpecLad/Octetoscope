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
import org.scalatest.MustMatchers._
import ru.corrigendum.octetoscope.core.mocks.MockDissector

class DissectorDriverSuite extends FunSuite {
  test("success") {
    val driver = getDissectorDriver(Function.const(Some(MockDissector)))
    driver(DissectorDriverSuite.FakeBlob) mustBe Atom(Bytes(5), new ToStringContents("magic"))
  }

  test("failure - file too small") {
    val driver = getDissectorDriver(Function.const(Some(new PlainDissector {
      override def dissect(context: DissectionContext, offset: InfoSize): PlainPiece =
        throw new IndexOutOfBoundsException
    })))

    a [TooSmallToDissectException] must be thrownBy driver(DissectorDriverSuite.FakeBlob)
  }

  test("failure - detection failed") {
    val driver = getDissectorDriver(Function.const(None))

    a [DetectionFailedException] must be thrownBy driver(DissectorDriverSuite.FakeBlob)
  }
}

object DissectorDriverSuite {
  val FakeBlob = new ArrayBlob("magic".getBytes(StandardCharsets.US_ASCII))
}
