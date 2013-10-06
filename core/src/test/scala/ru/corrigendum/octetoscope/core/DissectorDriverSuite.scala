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

import org.scalatest.matchers.ShouldMatchers._
import org.scalatest.FunSuite
import ru.corrigendum.octetoscope.core.mocks.{MockDissector, MockBinaryReader}
import java.nio.charset.StandardCharsets
import java.io.File
import ru.corrigendum.octetoscope.abstractinfra.Blob

class DissectorDriverSuite extends FunSuite {
  test("dissect empty") {
    val reader = new MockBinaryReader(Blob.empty)
    val driver = new DissectorDriverImpl(reader, MockDissector)
    driver.dissect(DissectorDriverSuite.FakePath) should equal (None)
  }

  test("dissect nonempty") {
    val reader = new MockBinaryReader(new ArrayBlob("magic".getBytes(StandardCharsets.US_ASCII)))
    val driver = new DissectorDriverImpl(reader, MockDissector)
    driver.dissect(DissectorDriverSuite.FakePath) should equal (Some(Atom(Bytes(5), Some("magic"))))
  }
}

object DissectorDriverSuite {
  val FakePath = new File("/abra/cadabra")
}
