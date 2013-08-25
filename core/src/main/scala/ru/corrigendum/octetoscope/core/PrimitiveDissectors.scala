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

import ru.corrigendum.octetoscope.abstractinfra.Blob
import java.nio.charset.StandardCharsets

object PrimitiveDissectors {
  object SInt32L extends Dissector {
    override def dissect(input: Blob, offset: Offset): Atom = {
      val Offset(bo) = offset

      val value = (input(bo) & 0xFF) |
        ((input(bo + 1) & 0xFF) << 8) |
        ((input(bo + 2) & 0xFF) << 16) |
        ((input(bo + 3) & 0xFF) << 24)

      Atom(Some(value.toString))
    }
  }

  class AsciiString private (length: Int) extends Dissector {
    override def dissect(input: Blob, offset: Offset): Piece = {
      val Offset(bo) = offset

      val value = new String(input.slice(bo, bo + length).toArray,
        StandardCharsets.US_ASCII)

      Atom(Some("\"" + value + "\""))
    }
  }

  object AsciiString {
    def apply(length: Int) = new AsciiString(length)
  }
}
