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
  object SInt32L extends Dissector[Int] {
    override def dissect(input: Blob, offset: Offset) = {
      val Offset(bo) = offset

      val value = (input(bo) & 0xFF) |
        ((input(bo + 1) & 0xFF) << 8) |
        ((input(bo + 2) & 0xFF) << 16) |
        ((input(bo + 3) & 0xFF) << 24)

      (Atom(32, Some(value.toString)), value)
    }
  }

  private object StringUtils {
    def decode(input: Blob, byteOffset: Long, length: Int) = {
      val value = new String(input.slice(byteOffset, byteOffset + length).toArray,
        StandardCharsets.US_ASCII)
      (Some("\"" + value + "\""), value)
    }
  }

  class AsciiString private (length: Int) extends Dissector[String] {
    override def dissect(input: Blob, offset: Offset) = {
      val Offset(bo) = offset

      val (repr, value) = StringUtils.decode(input, bo, length)

      (Atom(length * Offset.BitsPerByte, repr), value)
    }
  }

  object AsciiString {
    def apply(length: Int) = new AsciiString(length)
  }

  class AsciiZString private (length: Int) extends Dissector[String] {
    override def dissect(input: Blob, offset: Offset) = {
      val Offset(bo) = offset
      var actualLen = 0

      while (actualLen < length && input(bo + actualLen) != 0)
        actualLen += 1

      val (repr, value) = StringUtils.decode(input, bo, actualLen)

      (Atom(length * Offset.BitsPerByte, repr), value)
    }
  }

  object AsciiZString {
    def apply(length: Int) = new AsciiZString(length)
  }
}
