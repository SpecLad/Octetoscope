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

package ru.corrigendum.octetoscope.presentation.mocks

import java.io.File

import ru.corrigendum.octetoscope.abstractui.View

class MockView extends View {
  private[this] var _messageBoxes: List[(String, String)] = Nil

  def messageBoxes = _messageBoxes
  var selectedFile: Option[File] = None

  override def showMessageBox(text: String, title: String) {
    _messageBoxes :+= (text, title)
  }

  override def showFileOpenBox(): Option[File] = selectedFile
}
