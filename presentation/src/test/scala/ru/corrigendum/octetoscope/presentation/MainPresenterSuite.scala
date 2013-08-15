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

import ru.corrigendum.octetoscope.presentation.mocks.{MockDissectorDriver, MockDialogBoxer, MockMainView}
import org.scalatest.{BeforeAndAfter, FunSuite}
import ru.corrigendum.octetoscope.abstractui.MainView
import org.scalatest.matchers.MustMatchers._
import ru.corrigendum.octetoscope.presentation.tools.FakeMessageLocalizer
import ru.corrigendum.octetoscope.core.{Atom, VersionInfo}
import java.io.{IOException, File}

class MainPresenterSuite extends FunSuite with BeforeAndAfter {
  private[this] var view: MockMainView = _
  private[this] var boxer: MockDialogBoxer = _
  private[this] val strings: PresentationStrings = FakeMessageLocalizer.localize(classOf[PresentationStrings])
  private[this] var dissectorDriver: MockDissectorDriver = _

  before {
    view = new MockMainView()
    boxer = new MockDialogBoxer()
    dissectorDriver = new MockDissectorDriver(Atom("dummy"))
    new MainPresenter(strings, "Blarf", view, boxer, dissectorDriver)
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
    boxer.messages must equal (List(strings.appVersionString("Blarf", presentVersionInfo(VersionInfo.ours))))
  }

  test("open command - cancelled") {
    view.selectedFile = None
    view.trigger(MainView.CommandEvent(MainView.Command.Open))
    view.tabs must have size 0
  }

  test("open command - exception") {
    view.selectedFile = Some(MainPresenterSuite.fakePath)
    val exception = new IOException("whatever")
    dissectorDriver.exception = Some(exception)

    view.trigger(MainView.CommandEvent(MainView.Command.Open))

    view.tabs must have size 0
    boxer.messages must equal (List(strings.errorReadingFile(exception.getMessage)))
  }

  test("open command - successful") {
    view.selectedFile = Some(MainPresenterSuite.fakePath)
    view.trigger(MainView.CommandEvent(MainView.Command.Open))

    view.tabs must have size 1
    view.tabs.head.title must equal ("cadabra")
    view.tabs.head.toolTip must equal (MainPresenterSuite.fakePath.toString)
    view.tabs.head.tree must equal (presentPiece(dissectorDriver.dissect(MainPresenterSuite.fakePath)))
  }

  test("tab closing") {
    view.selectedFile = Some(MainPresenterSuite.fakePath)
    view.trigger(MainView.CommandEvent(MainView.Command.Open))

    view.tabs.head.trigger(MainView.TabClosedEvent)
    view.tabs must have size 0
  }
}

object MainPresenterSuite {
  private val fakePath = new File("/abra/cadabra")
}
