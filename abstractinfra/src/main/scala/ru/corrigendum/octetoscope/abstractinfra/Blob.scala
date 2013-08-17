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

package ru.corrigendum.octetoscope.abstractinfra

trait Blob {
  def apply(index: Long): Byte
  def size: Long
  def slice(start: Long, end: Long): Blob
  def toArray: Array[Byte]
}

object Blob {
  private val emptyArray = Array[Byte]()

  val empty: Blob = new Blob {
    override def apply(index: Long): Byte = throw new IndexOutOfBoundsException

    override def size: Long = 0

    override def slice(start: Long, end: Long): Blob =
      if (start == 0 && end == 0) empty
      else throw new IndexOutOfBoundsException

    override def toArray: Array[Byte] = emptyArray
  }
}
