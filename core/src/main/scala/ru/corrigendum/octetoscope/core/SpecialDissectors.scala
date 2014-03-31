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

object SpecialDissectors {
  private class Transformed[-OldValue, +NewValue](
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

  private class TransformedO[-OldValue, +NewValue](
    old: DissectorO[OldValue], transform: (Piece, OldValue) => (Piece, Option[NewValue])
  ) extends DissectorO[NewValue] {
    override def dissectO(input: Blob, offset: InfoSize): (Piece, Option[NewValue]) = {
      val (piece, valueO) = old.dissectO(input, offset)
      valueO match {
        case None => (piece, None)
        case Some(value) => transform(piece, value)
      }
    }
  }

  def transformedO[OldValue, NewValue](
    old: DissectorO[OldValue], transform: (Piece, OldValue) => (Piece, Option[NewValue])
  ): DissectorO[NewValue] = new TransformedO(old, transform)

  def constrained[Value](
    dissector: Dissector[Value], constraint: Constraint[Value], quality: Quality.Value
  ): Dissector[Value] = {
    def transform(piece: Piece, value: Value) =
      if (constraint.check(value)) (piece, value)
      else (piece.withNote(Note(quality, constraint.note(quality))), value)
    transformed(dissector, transform)
  }

  def constrainedO[Value](
    dissector: DissectorO[Value], constraint: Constraint[Value], quality: Quality.Value
  ): DissectorO[Value] = {
    def transform(piece: Piece, value: Value) =
      if (constraint.check(value)) (piece, Some(value))
      else (piece.withNote(Note(quality, constraint.note(quality))), Some(value))
    transformedO(dissector, transform)
  }

  def stronglyConstrainedO[Value](
    dissector: DissectorO[Value], constraint: Constraint[Value], quality: Quality.Value
  ): DissectorO[Value] = {
    def transform(piece: Piece, value: Value) =
      if (constraint.check(value)) (piece, Some(value))
      else (piece.withNote(Note(quality, constraint.note(quality))), None)
    transformedO(dissector, transform)
  }
}
