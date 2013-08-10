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

package ru.corrigendum.octetoscope.infra

import java.io.{FileInputStream, File, FileReader}
import ru.corrigendum.octetoscope.core.BinaryReader
import ru.corrigendum.octetoscope.core.Blob
import scala.collection.mutable
import resource.managed
import scala.util.control.Breaks._

object DefaultBinaryReader extends BinaryReader {
  def readWhole(path: File): Blob = {
    val buffer = new Array[Byte](1024 * 1024)
    val result = mutable.ArrayBuffer[Byte]()

    for (file <- managed(new FileInputStream(path))) {
      breakable { while (true) {
        val bytesRead = file.read(buffer)
        if (bytesRead == -1) break()
        result ++= buffer.take(bytesRead)
      }}
    }

    result
  }
}
