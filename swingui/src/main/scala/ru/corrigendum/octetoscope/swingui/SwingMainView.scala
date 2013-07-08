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
import javax.swing._
import java.awt.event.{ActionEvent, ActionListener, WindowEvent, WindowListener}
import ru.corrigendum.octetoscope.abstractui.MainView.Tab

private class SwingMainView(strings: UIStrings) extends SwingView with MainView {
  private[this] val menuBar = new JMenuBar()
  private[this] val tabs = new JTabbedPane()

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

    newMenuItem(strings.menuItemOpen(), menuFile, MainView.Command.Open)
    newMenuItem(strings.menuItemQuit(), menuFile, MainView.Command.Quit)
    newMenuItem(strings.menuItemAbout(), menuHelp, MainView.Command.About)
  }

  frame.setJMenuBar(menuBar)
  frame.setContentPane(tabs)
  frame.pack()

  override def dispose() {
    frame.dispose()
  }

  override def title: String = frame.getTitle

  override def title_=(title: String) {
    frame.setTitle(title)
  }

  override def show() {
    frame.setVisible(true)
  }

  override def addTab(title: String, toolTip: String): Tab = {
    val tab = new Tab {}
    tabs.addTab(title, null, new JLabel(tab.toString), toolTip)
    tab
  }
}
