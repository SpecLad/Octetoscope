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

package ru.corrigendum.octetoscope.presentation.mocks

import java.util.{Date, TimeZone}

import ru.corrigendum.octetoscope.abstractinfra.Clock

class MockClock(var timestamp: Date, var timeZone: TimeZone) extends Clock {
  var executionTime: Long = 0

  override def obtainTimestamp(): Date = timestamp.clone().asInstanceOf[Date]

  override def obtainTimeZone(): TimeZone = timeZone

  override def timeExecution[T](f: () => T): (Long, T) = (executionTime, f())
}
