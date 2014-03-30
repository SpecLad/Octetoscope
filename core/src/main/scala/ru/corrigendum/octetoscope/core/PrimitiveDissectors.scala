/*
  This file is part of Octetoscope.
  Copyright (C) 2013-2014 Octetoscope contributors (see /AUTHORS.txt)

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
import java.util.Locale

object PrimitiveDissectors {
  private val _numberAtomCache: Array[Array[Atom]] =
    Array.tabulate(4, 256) { (byteSize, value) => Atom(Bytes(byteSize + 1), Some(value.toString)) }

  private def getNumberAtom(byteSize: Int, value: Int) =
    if (byteSize > 0 && byteSize <= _numberAtomCache.length &&
        value >= 0 && value < _numberAtomCache(byteSize - 1).length)
      _numberAtomCache(byteSize - 1)(value)
    else
      Atom(Bytes(byteSize), Some(value.toString))

  private object SInt8 extends Dissector[Byte] {
    override def dissect(input: Blob, offset: InfoSize): (Piece, Byte) = {
      val Bytes(bo) = offset
      val value = input(bo)
      (getNumberAtom(1, value), value)
    }
  }

  def sInt8: Dissector[Byte] = SInt8

  private object UInt8 extends Dissector[Short] {
    override def dissect(input: Blob, offset: InfoSize): (Piece, Short) = {
      val Bytes(bo) = offset
      val byte = input(bo)
      val value = if (byte >= 0) byte else 256 + byte
      (getNumberAtom(1, value), value.toShort)
    }
  }

  def uInt8: Dissector[Short] = UInt8

  private object SInt16L extends Dissector[Short] {
    override def dissect(input: Blob, offset: InfoSize) = {
      val Bytes(bo) = offset

      val value = ((input(bo) & 0xFF) |
        ((input(bo + 1) & 0xFF) << 8)).toShort

      (getNumberAtom(2, value), value)
    }
  }

  def sInt16L: Dissector[Short] = SInt16L

  def readInt32L(input: Blob, offset: InfoSize) = {
    val Bytes(bo) = offset
    (input(bo) & 0xFF) |
      ((input(bo + 1) & 0xFF) << 8) |
      ((input(bo + 2) & 0xFF) << 16) |
      ((input(bo + 3) & 0xFF) << 24)
  }

  private object SInt32L extends Dissector[Int] {
    override def dissect(input: Blob, offset: InfoSize) = {
      val value = readInt32L(input, offset)

      (getNumberAtom(4, value), value)
    }
  }

  def sInt32L: Dissector[Int] = SInt32L

  private object Float32L extends Dissector[Float] {
    override def dissect(input: Blob, offset: InfoSize) = {
      val int = readInt32L(input, offset)
      val float = java.lang.Float.intBitsToFloat(int)

      if (float.isNaN)
        (Atom(Bytes(4), Some("%sNaN(0x%06x)".format(if (int < 0) "-" else "", int & 0x7FFFFF))), Float.NaN)
      else
        // 9 digits is the minimum number to ensure uniqueness
        (Atom(Bytes(4), Some("%.9g".formatLocal(Locale.ROOT, float))), float)
    }
  }

  def float32L: Dissector[Float] = Float32L

  abstract private class AsciiStringGeneric(length: Int) extends Dissector[String] {
    protected def findLength(input: Blob, byteOffset: Long): Int
    protected def assess(value: String): (PieceQuality.Value, Seq[String]) =
      (PieceQuality.Good, Nil)

    final override def dissect(input: Blob, offset: InfoSize) = {
      val Bytes(bo) = offset

      val value = new String(input.slice(bo, bo + findLength(input, bo)).toArray,
        StandardCharsets.US_ASCII)
      val (quality, notes) = assess(value)

      (Atom(Bytes(length), Some("\"" + value + "\""), quality, notes), value)
    }
  }

  private class AsciiString(length: Int) extends AsciiStringGeneric(length) {
    override protected def findLength(input: Blob, byteOffset: Long): Int = length
  }

  def asciiString(length: Int): Dissector[String] = new AsciiString(length)

  private class AsciiZString(length: Int) extends AsciiStringGeneric(length) {
    override protected def findLength(input: Blob, byteOffset: Long): Int = {
      var actualLen = 0

      while (actualLen < length && input(byteOffset + actualLen) != 0)
        actualLen += 1

      actualLen
    }

    override protected def assess(value: String): (PieceQuality.Value, Seq[String]) =
      if (value.length < length) super.assess(value)
      else (PieceQuality.Bad, Seq("missing NUL terminator"))
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
