/*
  This file is part of Octetoscope.
  Copyright (C) 2013, 2015 Octetoscope contributors (see /AUTHORS.txt)

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

class ArrayBlob(array: Array[Byte]) extends Blob {
  override def apply(index: Long): Byte = {
    // We do our own check instead of relying on the array's, to make sure
    // the index doesn't overflow when we convert it to int.
    if (index < 0 || index >= array.length) throw new IndexOutOfBoundsException
    array(index.toInt)
  }

  override def size: Long = array.length

  override def getRangeAsArray(start: Long, end: Long): Array[Byte] = {
    if (start < 0 || end > array.length || start > end) throw new IndexOutOfBoundsException
    array.slice(start.toInt, end.toInt)
  }
}
