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

package ru.corrigendum.octetoscope.presentation

import ru.corrigendum.octetoscope.abstractinfra.Translation

trait PresentationStrings {
  @Translation(format = "This is {0} version {1}.")
  def appVersionString(appName: String, version: String): String

  @Translation(format = "The file you have selected is empty.\nEmpty files can't be dissected.")
  def cantDissectEmptyFile(): String

  @Translation(format = "Failed to read file:\n{0}")
  def errorReadingFile(error: String): String
}

object PresentationStrings {
  val translationMap: Map[String, Class[_ <: PresentationStrings]] = Map("ru" -> classOf[Ru])

  trait Ru extends PresentationStrings {
    @Translation(format = "Это {0} версии {1}.")
    def appVersionString(appName: String, version: String): String
  }
}
