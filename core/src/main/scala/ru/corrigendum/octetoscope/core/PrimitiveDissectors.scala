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
  private val NumCachedSizes = 4
  private val NumCachedNumbers = 256

  private def makeNumberAtomCache[V: Numeric]: Array[Array[AtomCR[V]]] =
    Array.tabulate(NumCachedSizes, NumCachedNumbers) {
      (byteSize, value) => Atom(
        Bytes(byteSize + 1),
        new ToStringContents[V](implicitly[Numeric[V]].fromInt(value)))
    }

  private val byteAtomCache = makeNumberAtomCache[Byte]
  private val shortAtomCache = makeNumberAtomCache[Short]
  private val intAtomCache = makeNumberAtomCache[Int]

  private def numberIsCached(byteSize: Int, value: Int) =
    byteSize > 0 && byteSize <= NumCachedSizes &&
    value >= 0 && value < NumCachedNumbers

  private def getNumberAtom(byteSize: Int, value: Byte) =
    if (numberIsCached(byteSize, value))
      byteAtomCache(byteSize - 1)(value)
    else
      Atom(Bytes(byteSize), new ToStringContents[Byte](value))

  private def getNumberAtom(byteSize: Int, value: Short) =
    if (numberIsCached(byteSize, value))
      shortAtomCache(byteSize - 1)(value)
    else
      Atom(Bytes(byteSize), new ToStringContents[Short](value))

  private def getNumberAtom(byteSize: Int, value: Int) =
    if (numberIsCached(byteSize, value))
      intAtomCache(byteSize - 1)(value)
    else
      Atom(Bytes(byteSize), new ToStringContents[Int](value))

  private object SInt8 extends DissectorCR[Byte] {
    override def dissect(input: Blob, offset: InfoSize): AtomCR[Byte] = {
      val Bytes(bo) = offset
      val value = input(bo)
      getNumberAtom(1, value)
    }
  }

  def sInt8: DissectorCR[Byte] = SInt8

  private object UInt8 extends DissectorCR[Short] {
    override def dissect(input: Blob, offset: InfoSize): AtomCR[Short] = {
      val Bytes(bo) = offset
      val byte = input(bo)
      val value = if (byte >= 0) byte else 256 + byte
      getNumberAtom(1, value.toShort)
    }
  }

  def uInt8: DissectorCR[Short] = UInt8

  private object SInt16L extends DissectorCR[Short] {
    override def dissect(input: Blob, offset: InfoSize): AtomCR[Short] = {
      val Bytes(bo) = offset

      val value = ((input(bo) & 0xFF) |
        ((input(bo + 1) & 0xFF) << 8)).toShort

      getNumberAtom(2, value)
    }
  }

  def sInt16L: DissectorCR[Short] = SInt16L

  def readInt32L(input: Blob, offset: InfoSize) = {
    val Bytes(bo) = offset
    (input(bo) & 0xFF) |
      ((input(bo + 1) & 0xFF) << 8) |
      ((input(bo + 2) & 0xFF) << 16) |
      ((input(bo + 3) & 0xFF) << 24)
  }

  private object SInt32L extends DissectorCR[Int] {
    override def dissect(input: Blob, offset: InfoSize): AtomCR[Int] = {
      val value = readInt32L(input, offset)

      getNumberAtom(4, value)
    }
  }

  def sInt32L: DissectorCR[Int] = SInt32L

  private object Float32L extends DissectorCR[Float] {
    override def dissect(input: Blob, offset: InfoSize): AtomCR[Float] = {
      val int = readInt32L(input, offset)
      val float = java.lang.Float.intBitsToFloat(int)

      Atom(Bytes(4), new ContentsR[Float] {
        override val value: Float = float
        override def repr: String =
          if (float.isNaN) "%sNaN(0x%06x)".format(if (int < 0) "-" else "", int & 0x7FFFFF)
          else "%.9g".formatLocal(Locale.ROOT, float) // 9 digits is the minimum number to ensure uniqueness
      })
    }
  }

  def float32L: DissectorCR[Float] = Float32L

  abstract private class AsciiStringGeneric(length: Int) extends DissectorCR[String] {
    protected def findLength(input: Blob, byteOffset: Long): Int
    protected def assess(value: String): Seq[Note] = Nil

    final override def dissect(input: Blob, offset: InfoSize): AtomCR[String] = {
      val Bytes(bo) = offset

      val string = new String(input.slice(bo, bo + findLength(input, bo)).toArray,
        StandardCharsets.US_ASCII)

      Atom(
        Bytes(length),
        new ContentsR[String] {
          override val value: String = string
          override def repr: String = "\"" + value + "\""
        },
        assess(string))
    }
  }

  private class AsciiString(length: Int) extends AsciiStringGeneric(length) {
    override protected def findLength(input: Blob, byteOffset: Long): Int = length
  }

  def asciiString(length: Int): DissectorCR[String] = new AsciiString(length)

  private class AsciiZString(length: Int) extends AsciiStringGeneric(length) {
    override protected def findLength(input: Blob, byteOffset: Long): Int = {
      var actualLen = 0

      while (actualLen < length && input(byteOffset + actualLen) != 0)
        actualLen += 1

      actualLen
    }

    override protected def assess(value: String): Seq[Note] =
      if (value.length < length) super.assess(value)
      else Seq(Note(Quality.Bad, "missing NUL terminator"))
  }

  def asciiZString(length: Int): DissectorCR[String] = new AsciiZString(length)

  private class Magic(expected: Array[Byte], interpretation: String) extends DissectorC[Option[Unit]] {
    override def dissect(input: Blob, offset: InfoSize): AtomC[Option[Unit]] = {
      val Bytes(bo) = offset

      if (input.slice(bo, bo + expected.length).toArray.sameElements(expected)) {
        Atom(Bytes(expected.length), new Contents[Option[Unit]] {
          override val value: Option[Unit] = Some(())
          override def reprO: Option[String] = Some(interpretation)
        })
      } else {
        val note = "expected \"%s\" (0x%s)".format(interpretation, expected.map("%02x".format(_)).mkString)
        Atom(Bytes(expected.length), new Contents[Option[Unit]] {
          override val value: Option[Unit] = None
          override def reprO: Option[String] = None
        }, Seq(Note(Quality.Broken, note)))
      }
    }
  }

  def magic(expected: Array[Byte], interpretation: String): DissectorC[Option[Unit]] = new Magic(expected, interpretation)
}
