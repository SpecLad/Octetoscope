/*
  This file is part of Octetoscope.
  Copyright (C) 2013-2015 Octetoscope contributors (see /AUTHORS.txt)

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

package ru.corrigendum.octetoscope.abstractui

import ru.corrigendum.octetoscope.abstractinfra.Translation

trait UIStrings {
  @Translation(format = "&File")
  def menuFile(): String

  @Translation(format = "&Open...")
  def menuItemOpen(): String

  @Translation(format = "&Close")
  def menuItemClose(): String

  @Translation(format = "&Quit")
  def menuItemQuit(): String

  @Translation(format = "&Tools")
  def menuTools(): String

  @Translation(format = "Show &log")
  def menuItemShowLog(): String

  @Translation(format = "&Help")
  def menuHelp(): String

  @Translation(format = "&About")
  def menuItemAbout(): String

}

object UIStrings {
  val translationMap: Map[String, Class[_ <: UIStrings]] = Map("ru" -> classOf[Ru])

  private trait Ru extends UIStrings {
    @Translation(format = "В&ыход")
    override def menuItemQuit(): String
  }
}
