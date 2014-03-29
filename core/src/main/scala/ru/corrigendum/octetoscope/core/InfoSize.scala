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

sealed class InfoSize(val bytes: Long = 0) extends Ordered[InfoSize] {
  require(bytes >= 0)

  def totalBits: Long = bytes * InfoSize.BitsPerByte
  def - (that: InfoSize): InfoSize = InfoSize(this.bytes - that.bytes)
  def + (that: InfoSize): InfoSize = InfoSize(this.bytes + that.bytes)

  override def compare(that: InfoSize): Int = this.bytes.compareTo(that.bytes)

  override def equals(obj: Any): Boolean = obj match {
    case that: InfoSize => this.bytes == that.bytes
    case _ => false
  }

  override def hashCode(): Int = bytes.hashCode()

  override def toString: String = "Bytes(%d)".format(bytes)
}

object InfoSize {
  def apply(bytes: Long = 0): InfoSize = if (bytes < _cache.length) _cache(bytes.toInt) else new InfoSize(bytes)
  def unapply(size: InfoSize): Option[Long] = Some(size.bytes)
  val BitsPerByte: Long = 8

  private val _cache: Array[InfoSize] = Array.tabulate[InfoSize](256) { new InfoSize(_) }
}

object Bytes {
  def apply(bytes: Long) = InfoSize(bytes)
  def unapply(size: InfoSize) = InfoSize.unapply(size)
}
