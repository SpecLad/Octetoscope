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

package ru.corrigendum.octetoscope.abstractui

import java.awt.event.InputEvent
import javax.swing.KeyStroke

import scala.collection.mutable

trait MainView extends View with mutable.Publisher[MainView.Event] {
  override type Pub = MainView

  val logView: View

  def dispose(): Unit

  def numericViewWidth: Int
  def numericViewWidth_=(numCharacters: Int): Unit

  def setRawViewTexts(offsetViewText: String, numericViewText: String): Unit

  def addTab(title: String, toolTip: String, root: DisplayTreeNode): MainView.Tab

  def enableCommand(command: MainView.Command.Value): Unit
  def disableCommand(command: MainView.Command.Value): Unit

  def rawViewTopPixel: Int
  def scrollRawView(topPixel: Int): Unit

  def setNumericViewSelection(selectionStart: Int, selectionEnd: Int): Unit
}

object MainView {
  abstract sealed class Event
  case object ClosedEvent extends Event
  sealed case class CommandEvent(command: Command.Value) extends Event

  object Command extends Enumeration {
    val Open, Close, Quit = Value
    val ShowLog = Value
    val About = Value
  }

  private val SM = SubMenuDescription
  private val CI = CommandItemDescription
  val menuDescription = Seq(
    SM(_.menuFile(), Seq(
      CI(_.menuItemOpen(), Command.Open,
        shortcut = Some(KeyStroke.getKeyStroke('O', InputEvent.CTRL_DOWN_MASK))),
      CI(_.menuItemClose(), Command.Close,
        shortcut = Some(KeyStroke.getKeyStroke('W', InputEvent.CTRL_DOWN_MASK))),
      SeparatorDescription,
      CI(_.menuItemQuit(), Command.Quit)
    )),
    SM(_.menuTools(), Seq(
      CI(_.menuItemShowLog(), Command.ShowLog)
    )),
    SM(_.menuHelp(), Seq(
      CI(_.menuItemAbout(), Command.About)
    )))

  abstract sealed class TabEvent
  case object TabClosedEvent extends TabEvent
  case object TabActivatedEvent extends TabEvent

  trait Tab extends mutable.Publisher[MainView.TabEvent] {
    override type Pub = Tab

    def activate(): Unit // must publish a TabActivatedEvent even if the tab is already active
    def close(): Unit
  }
}
