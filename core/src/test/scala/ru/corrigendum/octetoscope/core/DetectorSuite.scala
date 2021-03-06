/*
  This file is part of Octetoscope.
  Copyright (C) 2014 Octetoscope contributors (see /AUTHORS.txt)

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
import org.scalatest.OptionValues._
import ru.corrigendum.octetoscope.core.mocks.MockDissector

class DetectorSuite extends FunSuite {
  test("apply") {
    val magicMap = Seq(Array[Byte](1, 2) -> MockDissector)
    val detector = getDetector(magicMap)

    detector(new ArrayBlob(Array[Byte]())) mustBe None
    detector(new ArrayBlob(Array[Byte](4, 5))) mustBe None
    detector(new ArrayBlob(Array[Byte](1, 2, 3))).value must be theSameInstanceAs MockDissector
  }
}
