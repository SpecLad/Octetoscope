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

package ru.corrigendum.octetoscope.swingui

import ru.corrigendum.octetoscope.abstractui.{DisplayTreeNode, UIStrings, MainView}
import javax.swing._
import java.awt.event.{ActionEvent, ActionListener, WindowEvent, WindowListener}
import ru.corrigendum.octetoscope.abstractui.MainView.{TabEvent, Tab}
import java.awt.Dimension
import javax.swing.event.{ChangeEvent, ChangeListener}

private class SwingMainView(strings: UIStrings, chooser: JFileChooser) extends SwingView(chooser) with MainView {
  private[this] val menuBar = new JMenuBar()
  private[this] val tabPane = new JTabbedPane()

  {
    val menuFile = new JMenu(strings.menuFile())
    val menuHelp = new JMenu(strings.menuHelp())

    menuBar.add(menuFile)
    menuBar.add(menuHelp)

    frame.addWindowListener(new WindowListener {
      override def windowDeiconified(e: WindowEvent) {}

      override def windowClosing(e: WindowEvent) {
        publish(MainView.ClosedEvent)
      }

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
  frame.setContentPane(tabPane)
  frame.setPreferredSize(new Dimension(800, 600))
  frame.pack()
  frame.setLocationRelativeTo(null)

  tabPane.addChangeListener(new ChangeListener {
    override def stateChanged(e: ChangeEvent): Unit = {
      val tabIndex = tabPane.getSelectedIndex
      if (tabIndex == -1) return

      val tabComponent = tabPane.getTabComponentAt(tabIndex).asInstanceOf[JComponent]
      if (tabComponent eq null) return // can happen when we haven't assigned the tab component yet

      val tab = tabComponent.getClientProperty(SwingMainView.PropertyKeyTab).asInstanceOf[TabImpl]
      tab.triggerEvent(MainView.TabActivatedEvent)
    }
  })

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

  override def addTab(title: String, toolTip: String, root: DisplayTreeNode): Tab = {
    lazy val tab: TabImpl = new TabImpl(TabComponent.get(title,
      () => tab.triggerEvent(MainView.TabClosedEvent)))

    tab.component.putClientProperty(SwingMainView.PropertyKeyTab, tab)

    val tree = new JTree(new PieceTreeNode(root))
    tree.setShowsRootHandles(true)
    tree.setCellRenderer(new PieceTreeCellRenderer)

    tabPane.addTab(null, null, new JScrollPane(tree), toolTip)
    tabPane.setTabComponentAt(tabPane.getTabCount - 1, tab.component)
    tabPane.setSelectedIndex(tabPane.getTabCount - 1)

    tab
  }

  private class TabImpl(val component: JComponent) extends Tab {
    def triggerEvent(event: TabEvent) { publish(event); }

    override def close() {
      tabPane.remove(tabPane.indexOfTabComponent(component))
    }
  }
}

private object SwingMainView {
  private val PropertyKeyTab: Object = new Object
}
