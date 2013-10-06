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

object PieceQuality extends Enumeration {
  val Good, Dubious, Bad, Broken = Value
}

abstract sealed class Piece {
  def size: InfoSize
  def repr: Option[String]
  def quality: PieceQuality.Value
  def notes: Seq[String]
}

sealed case class Atom(
  size: InfoSize,
  repr: Option[String],
  quality: PieceQuality.Value = PieceQuality.Good,
  notes: Seq[String] = List()
) extends Piece

sealed case class SubPiece(name: String, offset: InfoSize, piece: Piece)

sealed case class Molecule(
  size: InfoSize,
  repr: Option[String],
  children: Seq[SubPiece],
  quality: PieceQuality.Value = PieceQuality.Good,
  notes: Seq[String] = List()
) extends Piece
