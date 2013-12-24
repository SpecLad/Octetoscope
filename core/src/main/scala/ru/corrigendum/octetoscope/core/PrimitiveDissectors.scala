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
  private object SInt32L extends Dissector[Int] {
    override def dissect(input: Blob, offset: InfoSize) = {
      val Bytes(bo) = offset

      val value = (input(bo) & 0xFF) |
        ((input(bo + 1) & 0xFF) << 8) |
        ((input(bo + 2) & 0xFF) << 16) |
        ((input(bo + 3) & 0xFF) << 24)

      (Atom(Bytes(4), Some(value.toString)), value)
    }
  }

  def sInt32L: Dissector[Int] = SInt32L

  abstract private class AsciiStringGeneric(length: Int) extends Dissector[String] {
    def findLength(input: Blob, byteOffset: Long): Int

    final override def dissect(input: Blob, offset: InfoSize) = {
      val Bytes(bo) = offset

      val value = new String(input.slice(bo, bo + findLength(input, bo)).toArray,
        StandardCharsets.US_ASCII)

      (Atom(Bytes(length), Some("\"" + value + "\"")), value)
    }
  }

  private class AsciiString(length: Int) extends AsciiStringGeneric(length) {
    override def findLength(input: Blob, byteOffset: Long): Int = length
  }

  def asciiString(length: Int): Dissector[String] = new AsciiString(length)

  private class AsciiZString(length: Int) extends AsciiStringGeneric(length) {
    def findLength(input: Blob, byteOffset: Long): Int = {
      var actualLen = 0

      while (actualLen < length && input(byteOffset + actualLen) != 0)
        actualLen += 1

      actualLen
    }
  }

  def asciiZString(length: Int): Dissector[String] = new AsciiZString(length)

  private class Magic(expected: Array[Byte], interpretation: String) extends DissectorO[Unit] {
    override def dissectO(input: Blob, offset: InfoSize): (Piece, Option[Unit]) = {
      val Bytes(bo) = offset

      if (input.slice(bo, bo + expected.length).toArray.sameElements(expected)) {
        (Atom(Bytes(expected.length), Some(interpretation)), Some(()))
      } else {
        val note = "expected \"%s\" (0x%s)".format(interpretation, expected.map("%02x".format(_)).mkString)
        (Atom(Bytes(expected.length), None, PieceQuality.Broken, Seq(note)), None)
      }
    }
  }

  def magic(expected: Array[Byte], interpretation: String): DissectorO[Unit] = new Magic(expected, interpretation)
}
