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

package ru.corrigendum.octetoscope.core.mocks

import ru.corrigendum.octetoscope.abstractinfra.Blob
import ru.corrigendum.octetoscope.core._
import java.nio.charset.StandardCharsets
import ru.corrigendum.octetoscope.core.Atom
import scala.Some

object MockDissector extends Dissector[Unit] {
  override def dissect(input: Blob, offset: InfoSize) =
    (
      Atom(Bytes(input.size) - offset,
        Some(new String(input.slice(offset.bytes).toArray, StandardCharsets.US_ASCII))),
      Unit
    )
}
