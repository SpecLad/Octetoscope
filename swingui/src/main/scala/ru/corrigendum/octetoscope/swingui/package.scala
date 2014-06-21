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

import java.awt.event.{ActionEvent, ActionListener, KeyEvent}
import javax.swing._

import ru.corrigendum.octetoscope.abstractui._

package object swingui {
  private val MnemonicStringPattern = """^((?:[^&]|&&)*)&(\p{javaLetterOrDigit})((?:[^&]|&&)*)$""".r
  private val CommandTag = new Object

  private def splitMnemonic(ampString: String): (String, Int) = {
    val mnemonicMatchOpt = MnemonicStringPattern.findFirstMatchIn(ampString)
    assert(mnemonicMatchOpt.isDefined)

    val mnemonicMatch = mnemonicMatchOpt.get

    val beforeMnemonic = mnemonicMatch.group(1).replace("&&", "&")
    val mnemonic = mnemonicMatch.group(2)
    val afterMnemonic = mnemonicMatch.group(3).replace("&&", "&")

    (beforeMnemonic + mnemonic + afterMnemonic, beforeMnemonic.length)
  }

  def createMenuBarFromDescription[T](description: Seq[SubMenuDescription[T]],
                                      strings: UIStrings,
                                      invoke: T => Unit): JMenuBar = {
    val bar = new JMenuBar

    def createMenuItemFromDescription(description: MenuItemDescription[T]): Option[JMenuItem] = description match {
      case smd: SubMenuDescription[T] =>
        Some(createSubMenuFromDescription(smd))
      case ci: CommandItemDescription[T] =>
        val (text, mnemonicIndex) = splitMnemonic(ci.text(strings))
        val mi = new JMenuItem(text)
        mi.setMnemonic(KeyEvent.getExtendedKeyCodeForChar(text.charAt(mnemonicIndex)))
        mi.setDisplayedMnemonicIndex(mnemonicIndex)
        ci.shortcut.foreach(mi.setAccelerator)
        mi.addActionListener(new ActionListener {
          override def actionPerformed(e: ActionEvent): Unit = invoke(ci.command)
        })
        mi.putClientProperty(CommandTag, ci.command)
        Some(mi)
      case SeparatorDescription =>
        None
    }

    def createSubMenuFromDescription(description: SubMenuDescription[T]): JMenu = {
      val (text, mnemonicIndex) = splitMnemonic(description.text(strings))
      val menu = new JMenu(text)
      menu.setMnemonic(KeyEvent.getExtendedKeyCodeForChar(text.charAt(mnemonicIndex)))
      menu.setDisplayedMnemonicIndex(mnemonicIndex)

      for (item <- description.items)
        createMenuItemFromDescription(item).fold(menu.addSeparator())(menu.add(_))

      menu
    }

    for (menuDesc <- description)
      bar.add(createSubMenuFromDescription(menuDesc))

    bar
  }

  def findMenuItemsForCommand(bar: JMenuBar, command: Object): Seq[JMenuItem] = {
    def findInMenu(menu: JMenu): Seq[JMenuItem] = for {
      itemIndex <- 0 until menu.getItemCount
      item = menu.getItem(itemIndex)
      filteredItem <- item match {
        case subMenu: JMenu => findInMenu(subMenu)
        case null => Seq()
        case _ => if (item.getClientProperty(CommandTag) eq command) Seq(item) else Seq()
      }
    } yield item

    for {
      menuIndex <- 0 until bar.getMenuCount
      item <- findInMenu(bar.getMenu(menuIndex))
    } yield item
  }
}
