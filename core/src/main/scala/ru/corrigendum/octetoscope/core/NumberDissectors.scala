/*
  This file is part of Octetoscope.
  Copyright (C) 2014-2015 Octetoscope contributors (see /AUTHORS.txt)

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

import java.util.Locale

import ru.corrigendum.octetoscope.abstractinfra.Blob

private object NumberDissectors {
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

  object SInt8 extends DissectorCR[Byte] {
    override def dissect(context: DissectionContext, offset: InfoSize): AtomCR[Byte] = {
      val Bytes(bo) = offset
      val value = context.input(bo)
      getNumberAtom(1, value)
    }
  }

  object UInt8 extends DissectorCR[Short] {
    override def dissect(context: DissectionContext, offset: InfoSize): AtomCR[Short] = {
      val Bytes(bo) = offset
      val byte = context.input(bo)
      val value = if (byte >= 0) byte else 256 + byte
      getNumberAtom(1, value.toShort)
    }
  }

  object SInt16L extends DissectorCR[Short] {
    override def dissect(context: DissectionContext, offset: InfoSize): AtomCR[Short] = {
      val Bytes(bo) = offset

      val value = ((context.input(bo) & 0xFF) |
        ((context.input(bo + 1) & 0xFF) << 8)).toShort

      getNumberAtom(2, value)
    }
  }

  object UInt16L extends DissectorCR[Int] {
    override def dissect(context: DissectionContext, offset: InfoSize): Piece[ContentsR[Int]] = {
      val Bytes(bo) = offset

      val value = (context.input(bo) & 0xFF) |
        ((context.input(bo + 1) & 0xFF) << 8)

      getNumberAtom(2, value)
    }
  }

  private def readInt32L(input: Blob, offset: InfoSize) = {
    val Bytes(bo) = offset
    (input(bo) & 0xFF) |
      ((input(bo + 1) & 0xFF) << 8) |
      ((input(bo + 2) & 0xFF) << 16) |
      ((input(bo + 3) & 0xFF) << 24)
  }

  object SInt32L extends DissectorCR[Int] {
    override def dissect(context: DissectionContext, offset: InfoSize): AtomCR[Int] = {
      val value = readInt32L(context.input, offset)

      getNumberAtom(4, value)
    }
  }

  object Float32L extends DissectorCR[Float] {
    override def dissect(context: DissectionContext, offset: InfoSize): AtomCR[Float] = {
      val int = readInt32L(context.input, offset)
      val float = java.lang.Float.intBitsToFloat(int)

      Atom(Bytes(4), new ContentsR(float) {
        override def repr: String =
          if (value.isNaN) "%sNaN(0x%06x)".format(if (int < 0) "-" else "", int & 0x7FFFFF)
          else "%.9g".formatLocal(Locale.ROOT, value) // 9 digits is the minimum number to ensure uniqueness
      })
    }
  }
}
