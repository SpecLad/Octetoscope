/*
  This file is part of Octetoscope.
  Copyright (C) 2014 Octetoscope contributors (see /AUTHORS.txt)

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

import javax.swing.tree.DefaultMutableTreeNode
import ru.corrigendum.octetoscope.abstractui.DisplayTreeNode

private class PieceTreeNode(data: DisplayTreeNode) extends DefaultMutableTreeNode {
  val text = data.text
  val color = data.color

  private[this] var _data: Option[DisplayTreeNode] = if (data.children.isEmpty) None else Some(data)

  if (_data.isDefined)
    add(new DefaultMutableTreeNode("You shouldn't be seeing this"))

  def loadChildren(): Boolean = {
    for (data <- _data) {
      removeAllChildren()
      for (child <- data.children)
        add(new PieceTreeNode(child))
      _data = None
      return true
    }

    false
  }
}
