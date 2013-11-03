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

object SpecialDissectors {
  private class Transformed[OldValue, NewValue](
    old: Dissector[OldValue], transform: (Piece, OldValue) => (Piece, NewValue)
  ) extends Dissector[NewValue] {
    override def dissect(input: Blob, offset: InfoSize): (Piece, NewValue) = {
      val (piece, value) = old.dissect(input, offset)
      transform(piece, value)
    }
  }

  def transformed[OldValue, NewValue](
    old: Dissector[OldValue], transform: (Piece, OldValue) => (Piece, NewValue)
  ): Dissector[NewValue] = new Transformed(old, transform)
}
