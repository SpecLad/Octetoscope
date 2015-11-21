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

import ru.corrigendum.octetoscope.abstractinfra.Blob

object SpecialDissectors {
  private class Transformed[V, C <: Contents[V]](
    old: Dissector[V, C], transform: Piece[C] => Piece[C]
  ) extends Dissector[V, C] {
    override def dissect(input: Blob, offset: InfoSize): Piece[C] = transform(old.dissect(input, offset))
  }

  def transformed[V, C <: Contents[V]](
    old: Dissector[V, C], transform: Piece[C] => Piece[C]
  ): Dissector[V, C] = new Transformed(old, transform)

  def constrained[V, C <: Contents[V]](
    dissector: Dissector[V, C], constraint: Constraint[V], severity: NoteSeverity.Value
  ): Dissector[V, C] = {
    transformed(dissector, (piece: Piece[C]) => constraint.apply(piece, severity))
  }
}
