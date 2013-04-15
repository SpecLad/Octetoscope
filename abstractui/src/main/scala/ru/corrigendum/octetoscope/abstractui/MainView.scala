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

package ru.corrigendum.octetoscope.abstractui

import collection.mutable

trait MainView extends View with mutable.Publisher[MainView.Event] {
  override type Pub = MainView

  def dispose()

  def title: String
  def title_=(title: String)

  def show()
}

object MainView {
  abstract sealed class Event()
  sealed case class ClosedEvent() extends Event
  sealed case class CommandEvent(command: Command.Value) extends Event

  object Command extends Enumeration {
    val Quit, About = Value
  }
}