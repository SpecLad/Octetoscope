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

import java.nio.charset.StandardCharsets

import org.scalatest.FunSuite
import org.scalatest.MustMatchers._

class DissectorDriverSuite extends FunSuite {
  test("success") {
    val expectedResult = Atom(Bytes(5), EmptyContents)
    val untested = () => ()
    val driver = getDissectorDriver(Function.const(Some(new PlainDissector {
      override def dissect(context: DissectionContext, offset: InfoSize): Piece[Contents[Any]] = {
        context.input must be theSameInstanceAs DissectorDriverSuite.FakeBlob
        context.softLimit mustBe Bytes(DissectorDriverSuite.FakeBlob.size)
        context.untested must be theSameInstanceAs untested
        expectedResult
      }
    })))
    driver(DissectorDriverSuite.FakeBlob, untested) must be theSameInstanceAs expectedResult
  }

  test("failure - file too small") {
    val driver = getDissectorDriver(Function.const(Some(new PlainDissector {
      override def dissect(context: DissectionContext, offset: InfoSize): PlainPiece =
        throw new IndexOutOfBoundsException
    })))

    a [TooSmallToDissectException] must be thrownBy driver(DissectorDriverSuite.FakeBlob, DissectionContext.ignoreUntested)
  }

  test("failure - detection failed") {
    val driver = getDissectorDriver(Function.const(None))

    a [DetectionFailedException] must be thrownBy driver(DissectorDriverSuite.FakeBlob, DissectionContext.ignoreUntested)
  }
}

object DissectorDriverSuite {
  val FakeBlob = new ArrayBlob("magic".getBytes(StandardCharsets.US_ASCII))
}
