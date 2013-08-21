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

class ArrayBlob private (array: Array[Byte], offset: Int, length: Int) extends Blob {
  def this(array: Array[Byte]) = this(array, 0, array.length)

  override def apply(index: Long): Byte = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException
    array(offset + index.toInt)
  }

  override def size: Long = length

  override def slice(start: Long, end: Long): Blob = {
    if (start < 0 || end > length || start > end) throw new IndexOutOfBoundsException
    new ArrayBlob(array, offset + start.toInt, end.toInt - start.toInt)
  }

  override def toArray: Array[Byte] = array.slice(offset, offset + length)
}
