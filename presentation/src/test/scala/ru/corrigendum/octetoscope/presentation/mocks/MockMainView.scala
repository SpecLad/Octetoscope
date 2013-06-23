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

package ru.corrigendum.octetoscope.presentation.mocks

import ru.corrigendum.octetoscope.abstractui.MainView
import java.io.File
import scala.collection.mutable

class MockMainView extends MockView with MainView {
  private[this] var _disposed: Boolean = false
  private[this] var _visible: Boolean = false
  private[this] val _tabs = mutable.Buffer[(String, String)]()

  def disposed = _disposed
  def visible = _visible
  def tabs = _tabs.readOnly

  def dispose() {
    _disposed = true
  }

  def trigger(event: MainView.Event) {
    publish(event)
  }

  var title: String = ""

  def show() {
    _visible = true
  }

  def addTab(title: String, toolTip: String, baton: AnyRef) {
    _tabs += ((title, toolTip))
  }
}
