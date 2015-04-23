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
  private[this] var _numericViewText: String = ""
  private[this] var _offsetViewText: String = ""
  private[this] var _numericViewSelectionStart: Int = _
  private[this] var _numericViewSelectionEnd: Int = _

  def disposed = _disposed
  def visible = _visible
  def tabs: Seq[MockTab] = _tabs
  def numericViewText: String = _numericViewText
  def offsetViewText: String = _offsetViewText
  def numericViewSelectionStart: Int = _numericViewSelectionStart
  def numericViewSelectionEnd: Int = _numericViewSelectionEnd

  def dispose(): Unit = {
    _disposed = true
  }

  def trigger(event: MainView.Event): Unit = {
    publish(event)
  }

  override var title: String = ""
  override var numericViewWidth: Int = 0
  override def rawViewTopPixel = _rawViewTopPixel
  def rawViewTopPixel_=(value: Int): Unit = { _rawViewTopPixel = value }

  override def setRawViewTexts(offsetViewText: String, numericViewText: String): Unit = {
    this._offsetViewText = offsetViewText
    this._numericViewText = numericViewText
  }

  override def show(): Unit = {
    _visible = true
  }

  override def addTab(title: String, toolTip: String, root: DisplayTreeNode): MainView.Tab = {
    _tabs += new MockTab(title, toolTip, root)
    _tabs.last
  }

  def activeTab: Option[MockTab] = if (_activeTabIndex >= 0) Some(_tabs(_activeTabIndex)) else None

  def activateTab(newActiveIndex: Int): Unit = {
    _activeTabIndex = newActiveIndex
    if (_activeTabIndex >=0)
      _tabs(_activeTabIndex).trigger(MainView.TabActivatedEvent)
  }

  override def enableCommand(command: MainView.Command.Value): Unit = { _disabledCommands -= command }

  override def disableCommand(command: MainView.Command.Value): Unit = { _disabledCommands += command }

  def isCommandEnabled(command: MainView.Command.Value) = !_disabledCommands.contains(command)

  override def scrollRawView(topPixel: Int): Unit = { _rawViewTopPixel = topPixel }

  override def setNumericViewSelection(selectionStart: Int, selectionEnd: Int): Unit = {
    _numericViewSelectionStart = selectionStart
    _numericViewSelectionEnd = selectionEnd
  }

  class MockTab(val title: String, val toolTip: String, val tree: DisplayTreeNode) extends Tab {
    override def activate(): Unit = {
      activateTab(_tabs.indexOf(this))
    }

    override def close(): Unit = {
      _tabs -= this
      if (_tabs.isEmpty) {
        activateTab(-1)
      } else {
        activateTab(0)
      }
    }

    def trigger(event: MainView.TabEvent): Unit = {
      publish(event)
    }
  }
}
