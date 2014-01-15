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

class SequentialAdder(blob: Blob, initialOffset: InfoSize, builder: MoleculeBuilder) {
  var internalOffset = InfoSize()

  private def handleOutOfBounds[T](name: String, body: => T) =
    try {
      body
    } catch {
      case e: IndexOutOfBoundsException =>
        throw new MoleculeBuilderDissector.TruncatedException(e, name)
    }

  def apply[Value](name: String, dissector: DissectorO[Value]): Option[Value] = {
    val (piece, value) = handleOutOfBounds(name, dissector.dissectO(blob, initialOffset + internalOffset))
    builder.addChild(name, internalOffset, piece)
    internalOffset += piece.size
    value
  }

  def apply[Value](name: String, dissector: Dissector[Value]): Value = {
    val (piece, value) = handleOutOfBounds(name, dissector.dissect(blob, initialOffset + internalOffset))
    builder.addChild(name, internalOffset, piece)
    internalOffset += piece.size
    value
  }
}
