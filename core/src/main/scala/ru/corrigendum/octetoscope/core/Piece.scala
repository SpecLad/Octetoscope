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

object Quality extends Enumeration {
  val Good, Dubious, Bad, Broken = Value
}

sealed case class Note(pieceQuality: Quality.Value, text: String)

abstract class Contents[+V](final val value: V) {
  def reprO: Option[String]

  final override def equals(obj: Any): Boolean = obj match {
    case that: Contents[Any] => this.value == that.value && this.reprO == that.reprO
    case _ => false
  }

  final override def hashCode(): Int = (41 + value.hashCode()) * 41 + reprO.hashCode()

  override def toString: String = "Contents(%s, %s)".format(value, reprO)
}

abstract class ContentsR[+V](value: V) extends Contents[V](value) {
  final override def reprO: Option[String] = Some(repr)
  def repr: String
}

object EmptyContents extends Contents[Unit](()) {
  override def reprO: Option[String] = None
}

sealed class ToStringContents[+V](v: V) extends ContentsR[V](v) {
  override def repr: String = v.toString
}

sealed class EagerContents[+V](v: V, r: Option[String] = None) extends Contents[V](v) {
  override def reprO: Option[String] = r
}

sealed class EagerContentsR[+V](v: V, r: String) extends ContentsR[V](v) {
  override def repr: String = r
}

abstract sealed class Piece[+C <: Contents[Any]] {
  def size: InfoSize
  def contents: C
  def notes: Seq[Note]

  def withNote(note: Note): Piece[C]
}

sealed case class Atom[+C <: Contents[Any]](
  size: InfoSize,
  contents: C,
  notes: Seq[Note] = List()
) extends Piece[C] {
  override def withNote(note: Note): Atom[C] = copy(notes = notes :+ note)
}

sealed case class SubPiece(name: String, offset: InfoSize, piece: PlainPiece)

sealed case class Molecule[+C <: Contents[Any]](
  size: InfoSize,
  contents: C,
  children: Seq[SubPiece],
  notes: Seq[Note] = List()
) extends Piece[C] {
  override def withNote(note: Note): Molecule[C] = copy(notes = notes :+ note)
}
