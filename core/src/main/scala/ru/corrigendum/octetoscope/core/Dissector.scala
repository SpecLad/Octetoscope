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

// A dissector that may or may not return a value. The O is for Option.
trait DissectorO[+Value] {
  def dissectO(input: Blob, offset: InfoSize = InfoSize()): (Piece, Option[Value])

  def +?(constraint: Constraint[Value]): DissectorO[Value] =
    SpecialDissectors.constrainedO(this, constraint, Quality.Dubious)
  def +(constraint: Constraint[Value]): DissectorO[Value] =
    SpecialDissectors.constrainedO(this, constraint, Quality.Bad)
  def +!(constraint: Constraint[Value]): DissectorO[Value] =
    SpecialDissectors.stronglyConstrainedO(this, constraint, Quality.Bad)
}

// A dissector that will always return a value.
trait Dissector[+Value] extends DissectorO[Value] {
  final override def dissectO(input: Blob, offset: InfoSize = InfoSize()): (Piece, Option[Value]) = {
    val (piece, value) = dissect(input, offset)
    (piece, Some(value))
  }

  def dissect(input: Blob, offset: InfoSize = InfoSize()): (Piece, Value)

  override def +?(constraint: Constraint[Value]): Dissector[Value] =
    SpecialDissectors.constrained(this, constraint, Quality.Dubious)
  override def +(constraint: Constraint[Value]): Dissector[Value] =
    SpecialDissectors.constrained(this, constraint, Quality.Bad)
}
