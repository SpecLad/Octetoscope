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
import javax.swing.{JMenuItem, JMenu, JMenuBar}
import java.awt.event.{ActionEvent, ActionListener, WindowEvent, WindowListener}

private class SwingMainView(strings: UIStrings) extends SwingView with MainView {
  private[this] val menuBar = new JMenuBar()

  frame.setJMenuBar(menuBar)

  {
    val menuFile = new JMenu(strings.menuFile())
    val menuHelp = new JMenu(strings.menuHelp())

    menuBar.add(menuFile)
    menuBar.add(menuHelp)

    frame.addWindowListener(new WindowListener {
      override def windowDeiconified(e: WindowEvent) {}

      override def windowClosing(e: WindowEvent) { publish(MainView.ClosedEvent()) }

      override def windowClosed(e: WindowEvent) {}
      override def windowActivated(e: WindowEvent) {}
      override def windowOpened(e: WindowEvent) {}
      override def windowDeactivated(e: WindowEvent) {}
      override def windowIconified(e: WindowEvent) {}
    })

    def newMenuItem(title: String, menu: JMenu, command: MainView.Command.Value) {
      val item = new JMenuItem(title)
      item.addActionListener(new ActionListener {
        override def actionPerformed(e: ActionEvent) { publish(MainView.CommandEvent(command)) }
      })
      menu.add(item)
    }

    newMenuItem(strings.menuItemQuit(), menuFile, MainView.Command.Quit)
    newMenuItem(strings.menuItemAbout(), menuHelp, MainView.Command.About)
  }

  frame.pack()

  def dispose() {
    frame.dispose()
  }

  def title: String = frame.getTitle

  def title_=(title: String) {
    frame.setTitle(title)
  }

  def show() {
    frame.setVisible(true)
  }
}
