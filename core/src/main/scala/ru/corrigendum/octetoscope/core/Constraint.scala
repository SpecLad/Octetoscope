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

trait Constraint[-Value] {
  def check(value: Value): Boolean
  def note(quality: Quality.Value): String

  final def apply[C <: Contents[Value]](piece: Piece[C], quality: Quality.Value): Piece[C] =
    if (check(piece.contents.value)) piece
    else piece.withNote(Note(quality, note(quality)))
}

trait ShouldMustConstraint[-Value] extends Constraint[Value] {
  final override def note(quality: Quality.Value): String =
    if (quality.id <= Quality.Dubious.id) shouldNote
    else mustNote

  def shouldNote: String
  def mustNote: String
}
