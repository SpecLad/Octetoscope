/*
  This file is part of Octetoscope.
  Copyright (C) 2013-2014 Octetoscope contributors (see /AUTHORS.txt)

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

import java.io.{File, IOException}

import org.scalatest.LoneElement._
import org.scalatest.MustMatchers._
import org.scalatest.{BeforeAndAfter, FunSuite}
import ru.corrigendum.octetoscope.abstractinfra.Blob
import ru.corrigendum.octetoscope.abstractui.MainView
import ru.corrigendum.octetoscope.core._
import ru.corrigendum.octetoscope.presentation.mocks.{MockBinaryReader, MockDialogBoxer, MockDissectorDriver, MockMainView}
import ru.corrigendum.octetoscope.presentation.tools.{DisplayTreeNodeData, FakeMessageLocalizer}

class MainPresenterSuite extends FunSuite with BeforeAndAfter {
  private[this] var view: MockMainView = _
  private[this] var boxer: MockDialogBoxer = _
  private[this] val strings: PresentationStrings = FakeMessageLocalizer.localize(classOf[PresentationStrings])
  private[this] var binaryReader: MockBinaryReader = _
  private[this] var dissectorDriver: MockDissectorDriver = _

  before {
    view = new MockMainView()
    boxer = new MockDialogBoxer()
    binaryReader = new MockBinaryReader()
    dissectorDriver = new MockDissectorDriver()
    new MainPresenter(strings, "Blarf", view, boxer, binaryReader, dissectorDriver)
  }

  test("closing the window") {
    view.trigger(MainView.ClosedEvent)
    view mustBe 'disposed
  }

  test("initialization") {
    view mustBe 'visible
    view.title mustBe "Blarf"
    view.numericViewWidth mustBe 23
    view.isCommandEnabled(MainView.Command.Close) mustBe false
  }

  test("quit command") {
    view.trigger(MainView.CommandEvent(MainView.Command.Quit))
    view mustBe 'disposed
  }

  test("about command") {
    view.trigger(MainView.CommandEvent(MainView.Command.About))
    boxer.messages mustBe List(strings.appVersionString("Blarf", presentVersionInfo(VersionInfo.ours)))
  }

  test("open command - cancelled") {
    view.selectedFile = None
    view.trigger(MainView.CommandEvent(MainView.Command.Open))
    view.tabs must have size 0
  }

  test("open command - IOException") {
    view.selectedFile = Some(MainPresenterSuite.FakePath)
    val exception = new IOException("whatever")
    binaryReader.exception = Some(exception)

    view.trigger(MainView.CommandEvent(MainView.Command.Open))

    view.tabs must have size 0
    boxer.messages mustBe List(strings.errorReadingFile(exception.getMessage))
  }

  test("open command - TooSmallToDissectException") {
    view.selectedFile = Some(MainPresenterSuite.FakePath)
    dissectorDriver.exception = Some(new TooSmallToDissectException(null))

    view.trigger(MainView.CommandEvent(MainView.Command.Open))

    view.tabs must have size 0
    boxer.messages mustBe List(strings.fileTooSmallToDissect())
  }

  test("open command - DetectionFailedException") {
    view.selectedFile = Some(MainPresenterSuite.FakePath)
    dissectorDriver.exception = Some(new DetectionFailedException)

    view.trigger(MainView.CommandEvent(MainView.Command.Open))

    view.tabs must have size 0
    boxer.messages mustBe List(strings.cantDetectFileFormat())
  }

  test("open command - successful") {
    view.selectedFile = Some(MainPresenterSuite.FakePath)
    binaryReader.result = MainPresenterSuite.FakeBlob
    dissectorDriver.result = MainPresenterSuite.FakePiece
    view.trigger(MainView.CommandEvent(MainView.Command.Open))

    val tab = view.tabs.loneElement
    tab.title mustBe "cadabra"
    tab.toolTip mustBe MainPresenterSuite.FakePath.toString
    DisplayTreeNodeData.from(tab.tree) mustBe DisplayTreeNodeData.from(presentPiece(dissectorDriver(Blob.empty)))

    view.activeTab mustBe Some(tab)
    view.title mustBe "Blarf - cadabra"
    view.isCommandEnabled(MainView.Command.Close) mustBe true
    view.numericViewText mustBe presentBlobAsHexadecimal(MainPresenterSuite.FakeBlob, 8)
    view.offsetViewText mustBe generateBlobOffsets(MainPresenterSuite.FakeBlob.size, 8)
    view.rawViewTopPixel mustBe 0
  }

  test("switching tabs") {
    dissectorDriver.result = MainPresenterSuite.FakePiece
    view.selectedFile = Some(MainPresenterSuite.FakePath)
    binaryReader.result = MainPresenterSuite.FakeBlob
    view.trigger(MainView.CommandEvent(MainView.Command.Open))
    view.rawViewTopPixel = 65

    view.selectedFile = Some(new File(MainPresenterSuite.FakePath, "alakazam"))
    binaryReader.result = Blob.empty
    view.trigger(MainView.CommandEvent(MainView.Command.Open))

    view.activateTab(0)

    view.title mustBe "Blarf - cadabra"
    view.numericViewText mustBe presentBlobAsHexadecimal(MainPresenterSuite.FakeBlob, 8)
    view.offsetViewText mustBe generateBlobOffsets(MainPresenterSuite.FakeBlob.size, 8)
    view.rawViewTopPixel mustBe 65
  }

  test("tab closing via menu") {
    view.selectedFile = Some(MainPresenterSuite.FakePath)
    binaryReader.result = MainPresenterSuite.FakeBlob
    dissectorDriver.result = MainPresenterSuite.FakePiece
    view.trigger(MainView.CommandEvent(MainView.Command.Open))
    view.trigger(MainView.CommandEvent(MainView.Command.Close))

    view.tabs must have size 0
    view.title mustBe "Blarf"
    view.numericViewText mustBe ""
    view.offsetViewText mustBe ""
    view.isCommandEnabled(MainView.Command.Close) mustBe false
  }

  test("tab closing via close button") {
    view.selectedFile = Some(MainPresenterSuite.FakePath)
    binaryReader.result = MainPresenterSuite.FakeBlob
    dissectorDriver.result = MainPresenterSuite.FakePiece
    view.trigger(MainView.CommandEvent(MainView.Command.Open))

    view.tabs.head.trigger(MainView.TabClosedEvent)
    view.tabs must have size 0
    view.title mustBe "Blarf"
    view.numericViewText mustBe ""
    view.offsetViewText mustBe ""
    view.isCommandEnabled(MainView.Command.Close) mustBe false
  }
}

object MainPresenterSuite {
  private val FakePath = new File("/abra/cadabra")
  private val FakePiece = Atom(Bytes(5), new EagerContentsR((), "dummy"))
  private val FakeBlob = new ArrayBlob(Array.tabulate[Byte](19)(_.toByte))
}
