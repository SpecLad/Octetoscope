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

package ru.corrigendum.octetoscope.swingui

import ru.corrigendum.octetoscope.abstractui.{UIStrings, MainView}
import swing._
import event.{ActionEvent, WindowClosing}
import swing.Reactions.StronglyReferenced

private class SwingMainView(strings: UIStrings) extends SwingView with MainView {
  private[this] val menuBar = new MenuBar()
  private[this] val key = new AnyRef()

  frame.menuBar = menuBar

  {
    val menuFile = new Menu(strings.menuFile())
    val menuHelp = new Menu(strings.menuHelp())

    menuBar.contents += menuFile
    menuBar.contents += menuHelp

    val reactor = new Reactor with StronglyReferenced {
      reactions += {
        case WindowClosing(_) => {
          publish(MainView.ClosedEvent())
        }
        case ActionEvent(c: MenuItem) => {
          publish(MainView.CommandEvent(c.peer.getClientProperty(key).asInstanceOf[MainView.Command.Value]))
        }
      }
    }

    reactor.listenTo(frame)

    def newMenuItem(title: String, menu: Menu, command: MainView.Command.Value) {
      val item = new MenuItem(title)
      item.peer.putClientProperty(key, command)
      menu.contents += item
      reactor.listenTo(item)
    }

    newMenuItem(strings.menuItemQuit(), menuFile, MainView.Command.Quit)
    newMenuItem(strings.menuItemAbout(), menuHelp, MainView.Command.About)
  }

  frame.pack()

  def dispose() {
    frame.dispose()
  }

  def title: String = frame.title

  def title_=(title: String) {
    frame.title = title
  }

  def show() {
    frame.visible = true
  }
}
