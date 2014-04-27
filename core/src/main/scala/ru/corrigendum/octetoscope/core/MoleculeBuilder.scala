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

class MoleculeBuilder[+V](value: V) { mb =>
  private[this] val childrenBuilder = Seq.newBuilder[SubPiece]
  private[this] var contents: Contents[V] = new EagerContents(value)
  private[this] var autoSize: InfoSize = InfoSize()
  private[this] var fixedSize: Option[InfoSize] = None
  private[this] val notes = Seq.newBuilder[Note]
  private[this] var hasChildren_ = false

  def setRepr(repr: String) { this.contents = new EagerContents(value, Some(repr)) }

  def setReprLazyO(reprOption: => Option[String]) {
    this.contents = new Contents[V] {
      override val value: V = mb.value
      override def reprO: Option[String] = reprOption
    }
  }

  def setReprLazy(repr: => String) {
    setReprLazyO(Some(repr))
  }

  def addChild(name: String, offset: InfoSize, piece: PlainPiece) {
    childrenBuilder += SubPiece(name, offset, piece)
    autoSize = List(autoSize, offset + piece.size).max
    hasChildren_ = true
  }

  def addNote(quality: Quality.Value, text: String) { notes += Note(quality, text) }

  def build(): MoleculeC[V] = {
    Molecule(fixedSize.getOrElse(autoSize), contents, childrenBuilder.result(), notes.result())
  }

  def hasChildren: Boolean = hasChildren_

  def fixSize(size: InfoSize) { fixedSize = Some(size) }
}
