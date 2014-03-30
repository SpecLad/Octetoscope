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

import javax.swing.tree.{DefaultTreeCellRenderer, TreeCellRenderer}
import javax.swing.{BorderFactory, JLabel, JPanel, JTree}
import java.awt.{Color, FlowLayout, Component}

private class PieceTreeCellRenderer extends TreeCellRenderer {
  private val wrapped = new DefaultTreeCellRenderer
  wrapped.putClientProperty("html.disable", java.lang.Boolean.TRUE)

  private val panel = new JPanel(new FlowLayout(FlowLayout.LEADING, 3, 0))
  panel.setOpaque(false)
  private val noteBorder = BorderFactory.createCompoundBorder(
    BorderFactory.createLineBorder(Color.BLACK, 1),
    BorderFactory.createEmptyBorder(0, 2, 0, 2)
  )

  override def getTreeCellRendererComponent(
      tree: JTree, value: Any, selected: Boolean, expanded: Boolean,
      leaf: Boolean, row: Int, hasFocus: Boolean): Component = {
    val node = value.asInstanceOf[PieceTreeNode]
    val component = wrapped.getTreeCellRendererComponent(tree, node.text, selected, expanded, leaf, row, hasFocus)

    panel.removeAll()
    panel.add(component)

    for (note <- node.notes) {
      val label = new JLabel()

      label.setBorder(noteBorder)
      label.setBackground(note._1)
      label.setForeground(Color.BLACK)
      label.setOpaque(true)
      label.putClientProperty("html.disable", java.lang.Boolean.TRUE)
      // Have to set the text after the html.disable property, otherwise the former won't take effect.
      label.setText(note._2)

      panel.add(label)
    }

    panel
  }
}
