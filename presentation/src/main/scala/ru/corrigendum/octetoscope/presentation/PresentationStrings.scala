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

package ru.corrigendum.octetoscope.presentation

import ru.corrigendum.octetoscope.abstractinfra.Translation

trait PresentationStrings {
  // Error messages
  @Translation(format = "Failed to detect the selected file's format.")
  def errorCantDetectFileFormat(): String

  @Translation(format = "Failed to read file:\n{0}")
  def errorFailedToReadFile(error: String): String

  @Translation(format = "The file you have selected is too small to meaningfully dissect.")
  def errorFileTooSmallToDissect(): String

  // Log entries
  @Translation(format = "application started")
  def logEntryAppStarted(): String

  @Translation(format = "dissecting file \"{0}\"...")
  def logEntryDissectingFile(path: String): String

  @Translation(format = "failed to dissect file \"{0}\"")
  def logEntryFailedToDissectFile(path: String): String

  @Translation(format = "successfully dissected file \"{0}\"")
  def logEntrySuccessfullyDissectedFile(path: String): String

  // Miscellaneous UI elements
  @Translation(format = "This is {0} version {1}.")
  def aboutText(appName: String, version: String): String

  @Translation(format = "Log")
  def logViewTitle(): String
}

object PresentationStrings {
  val translationMap: Map[String, Class[_ <: PresentationStrings]] = Map("ru" -> classOf[Ru])

  trait Ru extends PresentationStrings {
    @Translation(format = "Это {0} версии {1}.")
    def aboutText(appName: String, version: String): String
  }
}
