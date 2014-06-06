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

import ru.corrigendum.octetoscope.abstractui._
import javax.swing._
import java.awt.event.{ActionEvent, ActionListener}
import ru.corrigendum.octetoscope.abstractui.CommandItemDescription
import ru.corrigendum.octetoscope.abstractui.SubMenuDescription

package object swingui {
  def createMenuBarFromDescription[T](description: Seq[SubMenuDescription[T]],
                                      strings: UIStrings,
                                      invoke: T => Unit): JMenuBar = {
    val bar = new JMenuBar

    def createMenuItemFromDescription(description: MenuItemDescription[T]): Option[JMenuItem] = description match {
      case smd: SubMenuDescription[T] =>
        Some(createSubMenuFromDescription(smd))
      case ci: CommandItemDescription[T] =>
        val mi = new JMenuItem(ci.text(strings))
        mi.addActionListener(new ActionListener {
          override def actionPerformed(e: ActionEvent): Unit = invoke(ci.command)
        })
        Some(mi)
      case SeparatorDescription =>
        None
    }

    def createSubMenuFromDescription(description: SubMenuDescription[T]): JMenu = {
      val menu = new JMenu(description.text(strings))

      for (item <- description.items)
        createMenuItemFromDescription(item).fold(menu.addSeparator())(menu.add(_))

      menu
    }

    for (menuDesc <- description)
      bar.add(createSubMenuFromDescription(menuDesc))

    bar
  }
}
