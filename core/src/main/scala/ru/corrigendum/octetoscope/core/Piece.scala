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

object Quality extends Enumeration {
  val Good, Dubious, Bad, Broken = Value
}

sealed case class Note(pieceQuality: Quality.Value, text: String)

abstract sealed class Piece {
  def size: InfoSize
  def repr: Option[String]
  def notes: Seq[Note]

  def withNote(note: Note): Piece
}

sealed case class Atom(
  size: InfoSize,
  repr: Option[String],
  notes: Seq[Note] = List()
) extends Piece {
  override def withNote(note: Note): Piece = copy(notes = notes :+ note)
}

sealed case class SubPiece(name: String, offset: InfoSize, piece: Piece)

sealed case class Molecule(
  size: InfoSize,
  repr: Option[String],
  children: Seq[SubPiece],
  notes: Seq[Note] = List()
) extends Piece {
  override def withNote(note: Note): Piece = copy(notes = notes :+ note)
}
