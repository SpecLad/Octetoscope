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

import java.io.{File, IOException}

import org.scalatest.LoneElement._
import org.scalatest.MustMatchers._
import org.scalatest.path
import ru.corrigendum.octetoscope.abstractinfra.Blob
import ru.corrigendum.octetoscope.abstractui.MainView
import ru.corrigendum.octetoscope.core._
import ru.corrigendum.octetoscope.presentation.mocks.{MockBinaryReader, MockDialogBoxer, MockDissectorDriver, MockMainView}
import ru.corrigendum.octetoscope.presentation.tools.{DisplayTreeNodeData, FakeMessageLocalizer}

class MainPresenterSuite extends path.FunSpec {
  describe("A MainPresenter") {
    val view = new MockMainView()
    val boxer = new MockDialogBoxer()
    val binaryReader = new MockBinaryReader()
    val dissectorDriver = new MockDissectorDriver()
    val strings: PresentationStrings = FakeMessageLocalizer.localize(classOf[PresentationStrings])

    MainPresenter.attach(strings, "Blarf", view, boxer, binaryReader, dissectorDriver)

    it("must put the window into the initial state") {
      view mustBe 'visible
      view.title mustBe "Blarf"
      view.numericViewWidth mustBe 23
      view.isCommandEnabled(MainView.Command.Close) mustBe false
      view.logView.title mustBe strings.logViewTitle()
      view.logView.entries.loneElement mustBe strings.logEntryAppStarted()
    }

    describe("when the window is closed") {
      view.trigger(MainView.ClosedEvent)
      it must behave like userQuits
    }

    describe("when the Quit command is given") {
      view.trigger(MainView.CommandEvent(MainView.Command.Quit))
      it must behave like userQuits
    }

    def userQuits(): Unit = {
      it("must dispose the window") {
        view mustBe 'disposed
      }
    }

    describe("when the About command is given") {
      view.trigger(MainView.CommandEvent(MainView.Command.About))
      it("must show the About message box") {
        boxer.messages mustBe List(strings.aboutText("Blarf", presentVersionInfo(VersionInfo.ours)))
      }
    }

    describe("when the Open command is given") {
      describe("and the Open dialog is cancelled") {
        view.selectedFile = None
        view.trigger(MainView.CommandEvent(MainView.Command.Open))

        it("must not open any tabs") {
          view.tabs must have size 0
        }
      }

      describe("and a file is selected") {
        view.selectedFile = Some(MainPresenterSuite.FakePath)

        describe("and an I/O exception occurs when reading the file") {
          val exception = new IOException("whatever")
          binaryReader.exception = Some(exception)
          view.trigger(MainView.CommandEvent(MainView.Command.Open))

          it("must complain and not open any tabs") {
            view.tabs must have size 0
            boxer.messages mustBe List(strings.errorFailedToReadFile(exception.getMessage))
          }
        }

        describe("and the file is too small to dissect") {
          dissectorDriver.exception = Some(new TooSmallToDissectException(null))
          view.trigger(MainView.CommandEvent(MainView.Command.Open))

          it("must complain and not open any tabs") {
            view.tabs must have size 0
            boxer.messages mustBe List(strings.errorFileTooSmallToDissect())
          }
        }

        describe("and the file's format can't be detected") {
          dissectorDriver.exception = Some(new DetectionFailedException)
          view.trigger(MainView.CommandEvent(MainView.Command.Open))

          it("must complain and not open any tabs") {
            view.tabs must have size 0
            boxer.messages mustBe List(strings.errorCantDetectFileFormat())
          }
        }

        describe("and the file is successfully dissected") {
          binaryReader.result = MainPresenterSuite.FakeBlob
          dissectorDriver.result = MainPresenterSuite.FakePiece
          view.trigger(MainView.CommandEvent(MainView.Command.Open))

          it("must open a tab for the file") {
            val tab = view.tabs.loneElement

            tab.title mustBe "cadabra"
            tab.toolTip mustBe MainPresenterSuite.FakePath.toString
            DisplayTreeNodeData.from(tab.tree) mustBe DisplayTreeNodeData.from(
              presentPiece(dissectorDriver(Blob.empty), (offset: InfoSize, size: InfoSize) => ()))

            view.activeTab mustBe Some(tab)
            view.title mustBe "Blarf - cadabra"
            view.isCommandEnabled(MainView.Command.Close) mustBe true
            view.numericViewText mustBe presentBlobAsHexadecimal(MainPresenterSuite.FakeBlob, 8)
            view.offsetViewText mustBe generateBlobOffsets(MainPresenterSuite.FakeBlob.size, 8)
            view.rawViewTopPixel mustBe 0
          }

          describe("when opening another tab and switching back") {
            view.rawViewTopPixel = 65

            view.selectedFile = Some(new File(MainPresenterSuite.FakePath, "alakazam"))
            binaryReader.result = Blob.empty
            view.trigger(MainView.CommandEvent(MainView.Command.Open))

            view.activateTab(0)

            it("must show the first file's contents again") {
              view.title mustBe "Blarf - cadabra"
              view.numericViewText mustBe presentBlobAsHexadecimal(MainPresenterSuite.FakeBlob, 8)
              view.offsetViewText mustBe generateBlobOffsets(MainPresenterSuite.FakeBlob.size, 8)
              view.rawViewTopPixel mustBe 65
            }
          }

          describe("when the Close command is given") {
            view.trigger(MainView.CommandEvent(MainView.Command.Close))
            it must behave like tabIsClosed
          }

          describe("when the tab's close button is clicked") {
            view.tabs.head.trigger(MainView.TabClosedEvent)
            it must behave like tabIsClosed
          }

          def tabIsClosed(): Unit = {
            it("must close the tab") {
              view.tabs must have size 0
              view.title mustBe "Blarf"
              view.numericViewText mustBe ""
              view.offsetViewText mustBe ""
              view.isCommandEnabled(MainView.Command.Close) mustBe false
            }
          }

          describe("when pieces are double-clicked") {
            it("must highlight the matching fragment in the raw view") {
              val tab = view.tabs.loneElement

              val expectedSelections = Seq(
                (6, 35), (0, 5), (1, 4), (1, 4),
                (9, 9), (9, 9), (10, 10), (10, 10))

              for ((child, expectedSelection) <- tab.tree.getChildren.get() zip expectedSelections) {
                child.eventListener.doubleClicked()
                (view.numericViewSelectionStart, view.numericViewSelectionEnd) mustBe expectedSelection
              }
            }
          }
        }
      }
    }

    describe("when the Show log command is given") {
      view.trigger(MainView.CommandEvent(MainView.Command.ShowLog))
      it("must show the log view") {
        view.logView mustBe 'visible
      }
    }
  }
}

object MainPresenterSuite {
  private val FakePath = new File("/abra/cadabra")
  private val FakeBlob = new ArrayBlob(Array.tabulate[Byte](19)(_.toByte))
  private val FakePiece = Molecule(Bytes(16), EmptyContents, Seq(
    SubPiece("alpha", Bytes(2), Atom(Bytes(10), EmptyContents)), // whole number of bytes
    SubPiece("beta", Bits(2), Atom(Bits(12), EmptyContents)), // covering more than half a byte (on each end)
    SubPiece("gamma", Bits(4), Atom(Bits(8), EmptyContents)), // covering exactly half a byte
    SubPiece("delta", Bits(6), Atom(Bits(4), EmptyContents)), // covering less than half a byte
    SubPiece("epsilon", Bytes(3), Atom(InfoSize(), EmptyContents)), // zero-length between bytes
    SubPiece("zeta", InfoSize(3, 2), Atom(InfoSize(), EmptyContents)), // zero-length in the middle of a left nibble
    SubPiece("eta", InfoSize(3, 4), Atom(InfoSize(), EmptyContents)), // zero-length between nibbles
    SubPiece("theta", InfoSize(3, 6), Atom(InfoSize(), EmptyContents)) // zero-length in the middle of a right nibble
  ))
}
