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

package ru.corrigendum.octetoscope.core.mocks

import java.nio.charset.StandardCharsets

import ru.corrigendum.octetoscope.core._

object MockDissector extends DissectorCR[String] {
  override def dissect(context: DissectionContext, offset: InfoSize) = {
    val str = new String(context.input.getRangeAsArray(offset.bytes), StandardCharsets.US_ASCII)

    Atom(Bytes(context.input.size) - offset, new ToStringContents(str))
  }
}
