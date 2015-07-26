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

import ru.corrigendum.octetoscope.presentation.Logger

import scala.collection.mutable

class MockLogger extends Logger {
  private[this] val _entries = mutable.ArrayBuffer[Seq[String]]()

  def entries: IndexedSeq[Seq[String]] = _entries

  def clear(): Unit = { _entries.clear() }

  override def log(mainEntry: String, otherEntries: String*): Unit = {
    _entries += mainEntry +: otherEntries
  }
}
