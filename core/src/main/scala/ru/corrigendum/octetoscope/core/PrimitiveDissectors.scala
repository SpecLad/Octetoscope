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

package ru.corrigendum.octetoscope.core

object PrimitiveDissectors {
  private class Padding(length: InfoSize) extends DissectorC[Unit] {
    override def dissect(context: DissectionContext, offset: InfoSize): Piece[Contents[Unit]] = {
      if (offset + length > Bytes(context.input.size))
        throw new IndexOutOfBoundsException
      Atom(length, EmptyContents)
    }
  }

  def padding(length: InfoSize): DissectorC[Unit] = new Padding(length)

  def sInt8: DissectorCR[Byte] = NumberDissectors.SInt8
  def uInt8: DissectorCR[Short] = NumberDissectors.UInt8
  def sInt16L: DissectorCR[Short] = NumberDissectors.SInt16L
  def uInt16L: DissectorCR[Int] = NumberDissectors.UInt16L
  def sInt32L: DissectorCR[Int] = NumberDissectors.SInt32L
  def float32L: DissectorCR[Float] = NumberDissectors.Float32L

  def asciiString(length: Int): DissectorCR[Option[String]] = new StringDissectors.AsciiString(length, false)
  def asciiZString(length: Int): DissectorCR[Option[String]] = new StringDissectors.AsciiZString(length, false)

  // ASCIIish strings are the same as ASCII, except undecodable characters are not considered an error.
  // Essentially, they're strings in an unknown encoding that's compatible with ASCII.
  def asciiishString(length: Int): DissectorCR[Option[String]] = new StringDissectors.AsciiString(length, true)
  def asciiishZString(length: Int): DissectorCR[Option[String]] = new StringDissectors.AsciiZString(length, true)

  private class Magic(expected: Array[Byte], interpretation: String) extends DissectorC[Option[Unit]] {
    override def dissect(context: DissectionContext, offset: InfoSize): AtomC[Option[Unit]] = {
      val Bytes(bo) = offset

      if (context.input.getRangeAsArray(bo, bo + expected.length).sameElements(expected)) {
        Atom(Bytes(expected.length), new EagerContents(Some(()), Some(interpretation)))
      } else {
        val note = "expected \"%s\" (0x%s)".format(interpretation, expected.map("%02x".format(_)).mkString)
        Atom(Bytes(expected.length), new EagerContents(None, None), Seq(Note(NoteSeverity.Failure, note)))
      }
    }
  }

  def magic(expected: Array[Byte], interpretation: String): DissectorC[Option[Unit]] = new Magic(expected, interpretation)

  private object Bit extends DissectorCR[Boolean] {
    override def dissect(context: DissectionContext, offset: InfoSize): PieceCR[Boolean] = {
      val byte = context.input(offset.bytes)
      val bit = (byte & (1 << (7 - offset.bits))) != 0

      Atom(Bits(1), new EagerContentsR(bit, if(bit) "True" else "False"))
    }
  }

  def bit: DissectorCR[Boolean] = Bit
}
