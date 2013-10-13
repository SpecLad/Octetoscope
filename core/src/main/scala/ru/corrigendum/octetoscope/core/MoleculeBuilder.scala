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

class MoleculeBuilder {
  private[this] val childrenBuilder = Seq.newBuilder[SubPiece]
  private[this] var repr: Option[String] = None
  private[this] var length: InfoSize = InfoSize()
  private[this] var quality: PieceQuality.Value = PieceQuality.Good
  private[this] val notes = Seq.newBuilder[String]

  def setRepr(repr: String) { this.repr = Some(repr) }

  def addChild(name: String, offset: InfoSize, piece: Piece) {
    childrenBuilder += SubPiece(name, offset, piece)
    length = List(length, offset + piece.size).max
  }

  def impair(newQuality: PieceQuality.Value) {
    if (newQuality.id > quality.id)
      quality = newQuality
  }

  def addNote(note: String) { notes += note }

  def build(): Molecule = Molecule(length, repr, childrenBuilder.result(), quality, notes.result())
}
