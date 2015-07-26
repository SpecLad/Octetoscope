/*
  This file is part of Octetoscope.
  Copyright (C) 2015 Octetoscope contributors (see /AUTHORS.txt)

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

package ru.corrigendum.octetoscope.presentation

import java.util.{GregorianCalendar, Locale, TimeZone}

import org.scalatest.LoneElement._
import org.scalatest.MustMatchers._
import org.scalatest.{FunSuite, OneInstancePerTest}
import ru.corrigendum.octetoscope.presentation.mocks.{MockClock, MockLogView}

class LoggerSuite extends FunSuite with OneInstancePerTest {
  private[this] val clock = {
    val calendar = new GregorianCalendar(TimeZone.getTimeZone("UTC"), Locale.ROOT)
    calendar.clear()
    calendar.set(1993, 8, 1, 15, 0)
    new MockClock(calendar.getTime, TimeZone.getTimeZone("GMT+02"))
  }

  private[this] val view = new MockLogView
  private[this] val logger: Logger = new LoggerImpl(clock, view)

  test("single entry") {
    logger.log("single entry")

    view.entries.loneElement mustBe "1993-09-01 17:00:00 - single entry"
  }

  test("multiple entries") {
    logger.log("main entry", "additional entry 1", "additional entry 2")

    view.entries mustBe Seq(
      "1993-09-01 17:00:00 - main entry",
      "                      additional entry 1",
      "                      additional entry 2"
    )
  }
}
