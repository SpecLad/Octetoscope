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

sealed class InfoSize(val bytes: Long = 0, val bits: Int = 0) extends Ordered[InfoSize] {
  require(bytes >= 0)
  require(bits >= 0 && bits < 8)

  def totalBits: Long = bytes * InfoSize.BitsPerByte + bits
  def - (that: InfoSize): InfoSize = Bits(this.totalBits - that.totalBits)
  def + (that: InfoSize): InfoSize = Bits(this.totalBits + that.totalBits)

  override def compare(that: InfoSize): Int = this.totalBits.compareTo(that.totalBits)

  override def equals(obj: Any): Boolean = obj match {
    case that: InfoSize => this.bytes == that.bytes && this.bits == that.bits
    case _ => false
  }

  override def hashCode(): Int = (527 + bytes.hashCode()) * 31 + bits.hashCode()

  override def toString: String =
    if (bits == 0) if (bytes == 0) "InfoSize()" else "Bytes(%d)".format(bytes)
    else if (bytes == 0) "Bits(%d)".format(bits) else "InfoSize(%d, %d)".format(bytes, bits)
}

object InfoSize {
  def apply(bytes: Long = 0, bits: Int = 0): InfoSize =
    if (bytes < _cache.length && bits == 0) _cache(bytes.toInt) else new InfoSize(bytes, bits)

  def unapply(size: InfoSize): Option[(Long, Int)] = Some((size.bytes, size.bits))
  val BitsPerByte: Long = 8

  private val _cache: Array[InfoSize] = Array.tabulate[InfoSize](256) { new InfoSize(_) }
}

object Bytes {
  def apply(bytes: Long) = InfoSize(bytes)
  def unapply(size: InfoSize): Option[Long] = if (size.bits == 0) Some(size.bytes) else None
}

object Bits {
  def apply(bits: Long) = InfoSize(bits / InfoSize.BitsPerByte, (bits % InfoSize.BitsPerByte).toInt)
  def unapply(size: InfoSize): Option[Long] = Some(size.totalBits)
}
