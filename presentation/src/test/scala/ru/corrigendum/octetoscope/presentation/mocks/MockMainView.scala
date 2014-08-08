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

import ru.corrigendum.octetoscope.abstractui.MainView.Tab
import ru.corrigendum.octetoscope.abstractui.{DisplayTreeNode, MainView}

import scala.collection.mutable

class MockMainView extends MockView with MainView {
  private[this] var _disposed: Boolean = false
  private[this] var _visible: Boolean = false
  private[this] val _tabs = mutable.Buffer[MockTab]()
  private[this] var _activeTabIndex: Int = -1
  private[this] var _disabledCommands = MainView.Command.ValueSet()
  private[this] var _rawViewTopPixel: Int = -1

  def disposed = _disposed
  def visible = _visible
  def tabs = _tabs.readOnly
  def rawViewTopPixel = _rawViewTopPixel

  def dispose() {
    _disposed = true
  }

  def trigger(event: MainView.Event) {
    publish(event)
  }

  override var title: String = ""
  override var numericViewWidth: Int = 0
  override var numericViewText: String = ""

  override def show() {
    _visible = true
  }

  override def addTab(title: String, toolTip: String, root: DisplayTreeNode): MainView.Tab = {
    _tabs += new MockTab(title, toolTip, root)
    _tabs.last
  }

  override def activeTab: Option[Tab] = if (_activeTabIndex >= 0) Some(_tabs(_activeTabIndex)) else None

  def activateTab(newActiveIndex: Int) {
    _activeTabIndex = newActiveIndex
    if (_activeTabIndex >=0)
      _tabs(_activeTabIndex).trigger(MainView.TabActivatedEvent)
  }

  override def enableCommand(command: MainView.Command.Value) { _disabledCommands -= command }

  override def disableCommand(command: MainView.Command.Value) { _disabledCommands += command }

  def isCommandEnabled(command: MainView.Command.Value) = !_disabledCommands.contains(command)

  override def scrollRawView(topPixel: Int) { _rawViewTopPixel = topPixel }

  class MockTab(val title: String, val toolTip: String, val tree: DisplayTreeNode) extends Tab {
    override def activate(): Unit = {
      activateTab(_tabs.indexOf(this))
    }

    override def close() {
      _tabs -= this
      if (_tabs.isEmpty) {
        activateTab(-1)
      } else {
        activateTab(0)
      }
    }

    def trigger(event: MainView.TabEvent) {
      publish(event)
    }
  }
}
