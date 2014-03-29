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
import javax.swing.JTree
import java.awt.Component

private class PieceTreeCellRenderer extends TreeCellRenderer {
  private val wrapped = new DefaultTreeCellRenderer
  wrapped.putClientProperty("html.disable", java.lang.Boolean.TRUE)

  override def getTreeCellRendererComponent(
      tree: JTree, value: Any, selected: Boolean, expanded: Boolean,
      leaf: Boolean, row: Int, hasFocus: Boolean): Component = {
    val node = value.asInstanceOf[PieceTreeNode]
    val component = wrapped.getTreeCellRendererComponent(tree, node.text, selected, expanded, leaf, row, hasFocus)
    component.setForeground(node.color)
    component
  }
}
