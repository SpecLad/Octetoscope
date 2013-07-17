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

import mocks.{MockDialogBoxer, MockMainView}
import org.scalatest.{BeforeAndAfter, FunSuite}
import ru.corrigendum.octetoscope.abstractui.MainView
import org.scalatest.matchers.MustMatchers._
import ru.corrigendum.octetoscope.presentation.tools.FakeMessageLocalizer
import ru.corrigendum.octetoscope.core.VersionInfo
import java.io.File

class MainPresenterSuite extends FunSuite with BeforeAndAfter {
  private[this] var view: MockMainView = _
  private[this] var boxer: MockDialogBoxer = _
  private[this] val strings: PresentationStrings = FakeMessageLocalizer.localize(classOf[PresentationStrings])

  before {
    view = new MockMainView()
    boxer = new MockDialogBoxer()
    new MainPresenter(strings, "Blarf", view, boxer)
  }

  test("closing the window") {
    view.trigger(MainView.ClosedEvent)
    view must be ('disposed)
  }

  test("initialization") {
    view must be ('visible)
    view.title must equal ("Blarf")
  }

  test("quit command") {
    view.trigger(MainView.CommandEvent(MainView.Command.Quit))
    view must be ('disposed)
  }

  test("about command") {
    view.trigger(MainView.CommandEvent(MainView.Command.About))
    boxer.messages must equal (List((view, strings.appVersionString("Blarf", formatVersionInfo(VersionInfo.ours)))))
  }

  test("open command") {
    view.selectedFile = None
    view.trigger(MainView.CommandEvent(MainView.Command.Open))
    view.tabs must have size 0

    val fakePath = new File("/abra/cadabra")

    view.selectedFile = Some(fakePath)
    view.trigger(MainView.CommandEvent(MainView.Command.Open))

    view.tabs must have size 1
    view.tabs.head.title must equal ("cadabra")
    view.tabs.head.toolTip must equal (fakePath.toString)
  }
}
