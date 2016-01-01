/*
  This file is part of Octetoscope.
  Copyright (C) 2013-2016 Octetoscope contributors (see /AUTHORS.txt)

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

object SpecialDissectors {
  private class Transformed[V, C <: Contents[V]](
    old: Dissector[V, C], transform: Piece[C] => Piece[C]
  ) extends Dissector[V, C] {
    override def dissect(context: DissectionContext, offset: InfoSize): Piece[C] =
      transform(old.dissect(context, offset))
  }

  def transformed[V, C <: Contents[V]](
    old: Dissector[V, C], transform: Piece[C] => Piece[C]
  ): Dissector[V, C] = new Transformed(old, transform)

  def constrained[V, C <: Contents[V]](
    dissector: Dissector[V, C], constraint: Constraint[V], severity: NoteSeverity.Value
  ): Dissector[V, C] = {
    transformed(dissector, (piece: Piece[C]) => constraint.apply(piece, severity))
  }

  private class FixedSize[V, C <: Contents[V]](
    underlying: DissectorWithDefaultValue[V, C], size: InfoSize
  ) extends DissectorWithDefaultValue[V, C] {
    override def defaultValue: V = underlying.defaultValue

    override def dissect(context: DissectionContext, offset: InfoSize): Piece[C] = {
      require(size <= Bytes(context.input.size) - offset)
      underlying.dissect(context.copy(softLimit = offset + size), offset).withSize(size)
    }
  }

  // While this can wrap any dissector, it should only be used with MoleculeDissectors.
  // A molecule's actual size can always be recalculated from its children, but if an
  // atom's size is forcibly changed, the original size is lost.
  // It's also safe to use this with dissectors whose output always ends exactly at the
  // soft limit, since such output's size won't need to be changed.
  def fixedSize[V, C <: Contents[V]](
    underlying: DissectorWithDefaultValue[V, C], size: InfoSize
  ): DissectorWithDefaultValue[V, C] = new FixedSize(underlying, size)
}
