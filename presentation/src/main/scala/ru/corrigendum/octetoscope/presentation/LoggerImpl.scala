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

import java.text.SimpleDateFormat
import java.util.Locale

import ru.corrigendum.octetoscope.abstractinfra.Clock
import ru.corrigendum.octetoscope.abstractui.LogView

class LoggerImpl(clock: Clock, view: LogView) extends Logger {
  override def log(mainEntry: String, otherEntries: String*): Unit = {
    val sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss - ", Locale.ROOT)
    sdf.setTimeZone(clock.obtainTimeZone())
    view.addEntry(sdf.format(clock.obtainTimestamp()) + mainEntry)

    if (otherEntries.nonEmpty) {
      val spaces = " " * sdf.toPattern.length

      for (otherEntry <- otherEntries)
        view.addEntry(spaces + otherEntry)
    }
  }
}
