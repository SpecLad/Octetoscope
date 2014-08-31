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

import java.awt._
import java.awt.event.{WindowEvent, WindowListener}
import javax.swing._
import javax.swing.event.{ChangeEvent, ChangeListener, TreeExpansionEvent, TreeWillExpandListener}
import javax.swing.text.DefaultCaret
import javax.swing.tree.DefaultTreeModel

import ru.corrigendum.octetoscope.abstractui.MainView.{TabActivatedEvent, Tab, TabEvent}
import ru.corrigendum.octetoscope.abstractui.{DisplayTreeNode, MainView, UIStrings}

private class SwingMainView(strings: UIStrings, chooser: JFileChooser) extends SwingView(chooser) with MainView {
  private[this] val tabPane = new JTabbedPane()

  private[this] val numericView = new JTextArea()
  numericView.setFont(new Font("Monospaced", Font.PLAIN, 14))
  numericView.setEditable(false)

  private[this] val offsetView = new JTextArea()
  offsetView.setFont(numericView.getFont)
  offsetView.setEnabled(false)
  offsetView.setText("00000000\n" * 100)

  private[this] val rawViewScroller = new JScrollPane(numericView)
  rawViewScroller.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
  // always show the vertical scrollbar, so that the view doesn't change size when tabs are switched
  rawViewScroller.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS)
  rawViewScroller.setBorder(BorderFactory.createMatteBorder(0, 1, 0, 0, Color.BLACK))
  rawViewScroller.setRowHeaderView(offsetView)
  rawViewScroller.setViewportBorder(BorderFactory.createMatteBorder(0, 2, 0, 0, new Color(225, 225, 225)))

  {
    val caret = new DefaultCaret
    caret.setUpdatePolicy(DefaultCaret.NEVER_UPDATE)
    numericView.setCaret(caret)
  }

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

  frame.setJMenuBar(createMenuBarFromDescription(MainView.menuDescription, strings,
    (c: MainView.Command.Value) => publish(MainView.CommandEvent(c))))
  frame.getContentPane.add(tabPane)
  frame.getContentPane.add(rawViewScroller, BorderLayout.EAST)

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

  override def numericViewWidth: Int = numericView.getColumns

  override def numericViewWidth_=(numCharacters: Int) {
    numericView.setColumns(numCharacters)
  }

  override def numericViewText: String = numericView.getText

  override def numericViewText_=(text: String) {
    numericView.setText(text)
    /* Changing the text will change the text area's preferred size, but
       it will not change its actual size until after the event is handled.
       However, we need the actual size to change now, since the presenter will
       most likely try to scroll the text afterwards, and the size needs to be
       correct for the scrolling to work. Thus, we ask the viewport to validate
       now, which sets the text area's size.
     */
    rawViewScroller.getViewport.validate()
  }

  override def show() {
    frame.setVisible(true)
  }

  override def addTab(title: String, toolTip: String, root: DisplayTreeNode): Tab = {
    lazy val tab: TabImpl = new TabImpl(TabComponent.get(title,
      () => tab.triggerEvent(MainView.TabClosedEvent)))

    tab.component.putClientProperty(SwingMainView.PropertyKeyTab, tab)

    val model = new DefaultTreeModel(new PieceTreeNode(root), true)
    val tree = new JTree(model)
    // we collapse the root and then expand it again, so that
    // the expand handler can fire for it, and its children are properly loaded
    tree.collapseRow(0)

    tree.setShowsRootHandles(true)
    tree.setCellRenderer(new PieceTreeCellRenderer)
    tree.addTreeWillExpandListener(new TreeWillExpandListener {
      override def treeWillCollapse(event: TreeExpansionEvent) {}
      override def treeWillExpand(event: TreeExpansionEvent) {
        val node = event.getPath.getLastPathComponent.asInstanceOf[PieceTreeNode]
        if (node.loadChildren()) model.nodeStructureChanged(node)
      }
    })
    tree.expandRow(0)

    tabPane.addTab(null, null, new JScrollPane(tree), toolTip)
    tabPane.setTabComponentAt(tabPane.getTabCount - 1, tab.component)

    tab
  }

  override def enableCommand(command: MainView.Command.Value) {
    findMenuItemsForCommand(frame.getJMenuBar, command).foreach(_.setEnabled(true))
  }

  override def disableCommand(command: MainView.Command.Value) {
    findMenuItemsForCommand(frame.getJMenuBar, command).foreach(_.setEnabled(false))
  }

  override def rawViewTopPixel: Int = rawViewScroller.getViewport.getViewPosition.getY.toInt

  override def scrollRawView(topPixel: Int) {
    rawViewScroller.getViewport.setViewPosition(new Point(0, topPixel))
  }

  private class TabImpl(val component: JComponent) extends Tab {
    def triggerEvent(event: TabEvent) { publish(event); }

    override def activate(): Unit = {
      val newSelectedIndex = tabPane.indexOfTabComponent(component)

      // setSelectedIndex doesn't do anything if the selected index doesn't
      // change. However, the presenter depends on the event always being fired
      // upon calling activate(), so call it manually.
      if (newSelectedIndex == tabPane.getSelectedIndex)
        publish(TabActivatedEvent)
      else
        tabPane.setSelectedIndex(newSelectedIndex)
    }

    override def close() {
      tabPane.remove(tabPane.indexOfTabComponent(component))
    }
  }
}

private object SwingMainView {
  private val PropertyKeyTab: Object = new Object
}
