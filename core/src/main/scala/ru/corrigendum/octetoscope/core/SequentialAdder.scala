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

class SequentialAdder(blob: Blob, initialOffset: InfoSize, builder: MoleculeBuilder[Any]) {
  var internalOffset = InfoSize()

  def apply[V](name: String, dissector: DissectorC[V]): V = {
    getContents(name, dissector).value
  }

  def filtered[V](name: String, dissector: DissectorC[V])(criticalConstraints: Constraint[V]*): Option[V] = {
    val actualDissector = criticalConstraints.foldLeft(dissector) { _ + _ }
    criticalConstraints.foldLeft[Option[V]](Some(apply(name, actualDissector))) { (opt, constraint) =>
      opt.filter(constraint.check)
    }
  }

  def getContents[V, C <: Contents[V]](name: String, dissector: Dissector[V, C]): C = {
    val piece = try {
      dissector.dissect(blob, initialOffset + internalOffset)
    } catch {
      case e: IndexOutOfBoundsException =>
        throw new MoleculeBuilderDissector.TruncatedException(e, name)
    }

    builder.addChild(name, internalOffset, piece)
    internalOffset += piece.size
    piece.contents
  }
}
