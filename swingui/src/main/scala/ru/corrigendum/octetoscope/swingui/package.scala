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

package ru.corrigendum.octetoscope

import ru.corrigendum.octetoscope.abstractui.{CommandItemDescription, MenuItemDescription, UIStrings, SubMenuDescription}
import javax.swing.{JMenuItem, JMenu, JMenuBar}
import java.awt.event.{ActionEvent, ActionListener}

package object swingui {
  def createMenuBarFromDescription[T](description: Seq[SubMenuDescription[T]],
                                      strings: UIStrings,
                                      invoke: T => Unit): JMenuBar = {
    val bar = new JMenuBar

    def createMenuItemFromDescription(description: MenuItemDescription[T]): JMenuItem = description match {
      case smd: SubMenuDescription[T] =>
        createSubMenuFromDescription(smd)
      case ci: CommandItemDescription[T] =>
        val mi = new JMenuItem(ci.text(strings))
        mi.addActionListener(new ActionListener {
          override def actionPerformed(e: ActionEvent): Unit = invoke(ci.command)
        })
        mi
    }

    def createSubMenuFromDescription(description: SubMenuDescription[T]): JMenu = {
      val menu = new JMenu(description.text(strings))

      for (item <- description.items)
        menu.add(createMenuItemFromDescription(item))

      menu
    }

    for (menuDesc <- description)
      bar.add(createSubMenuFromDescription(menuDesc))

    bar
  }
}
