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

import java.io.IOException

import ru.corrigendum.octetoscope.abstractinfra.BinaryReader
import ru.corrigendum.octetoscope.abstractui.MainView
import ru.corrigendum.octetoscope.abstractui.MainView._
import ru.corrigendum.octetoscope.core._

object MainPresenter {
  private val DefaultBytesPerRow = 8

  def attach(strings: PresentationStrings,
             appName: String,
             view: MainView,
             boxer: DialogBoxer,
             binaryReader: BinaryReader,
             dissectorDriver: DissectorDriver): Unit = {
    var numTabs = 0
    var currentTabHandler: Option[TabHandler] = None

    def closeTab(tab: MainView.Tab): Unit = {
      tab.close()
      numTabs -= 1
      if (numTabs == 0) {
        view.title = appName
        view.setRawViewTexts("", "")
        view.disableCommand(MainView.Command.Close)
        currentTabHandler = None
      }
    }

    def handlePieceDoubleClick(offset: InfoSize, size: InfoSize): Unit = {
      val selectionStart = if (offset.bits < 4) offset.bytes * 3 else offset.bytes * 3 + 1
      val selectionEnd = if (size == InfoSize()) {
        selectionStart
      } else {
        val offsetEnd = offset + size
        if (offsetEnd.bits == 0) offsetEnd.bytes * 3 - 1
        else if (offsetEnd.bits > 4) offsetEnd.bytes * 3 + 2
        else offsetEnd.bytes * 3 + 1
      }
      view.setNumericViewSelection(selectionStart.toInt, selectionEnd.toInt)
    }

    object ViewHandler extends MainView#Sub {
      override def notify(pub: MainView#Pub, event: Event): Unit = {
        event match {
          case ClosedEvent => pub.dispose()

          case CommandEvent(MainView.Command.About) =>
            boxer.showMessageBox(strings.appVersionString(appName, presentVersionInfo(VersionInfo.ours)))

          case CommandEvent(MainView.Command.Quit) => pub.dispose()

          case CommandEvent(MainView.Command.Open) =>
            pub.showFileOpenBox() match {
              case None =>
              case Some(path) =>
                val (blob, piece) = try {
                  val blob = binaryReader.readWhole(path)
                  (blob, dissectorDriver(blob))
                } catch {
                  case ioe: IOException =>
                    boxer.showMessageBox(strings.errorReadingFile(ioe.getMessage))
                    return
                  case _: TooSmallToDissectException =>
                    boxer.showMessageBox(strings.fileTooSmallToDissect())
                    return
                  case _: DetectionFailedException =>
                    boxer.showMessageBox(strings.cantDetectFileFormat())
                    return
                }

                val numericViewText = presentBlobAsHexadecimal(blob, MainPresenter.DefaultBytesPerRow)
                val offsetViewText = generateBlobOffsets(blob.size, MainPresenter.DefaultBytesPerRow)
                val newTab = pub.addTab(path.getName, path.toString, presentPiece(piece, handlePieceDoubleClick))
                numTabs += 1
                newTab.subscribe(new TabHandler(newTab, path.getName, numericViewText, offsetViewText))
                newTab.activate()
                view.enableCommand(MainView.Command.Close)
            }

          case CommandEvent(MainView.Command.Close) =>
            closeTab(currentTabHandler.get.tab)

          case CommandEvent(MainView.Command.ShowLog) =>
            view.logView.show()

          case CommandEvent(_) => // workaround for bug SI-7206
        }
      }
    }

    class TabHandler(val tab: Tab, title: String,
                     numericViewText: String, offsetViewText: String) extends MainView.Tab#Sub {
      private[this] var rawViewTopPixel: Int = 0

      private def saveRawViewTopPixel(): Unit = {
        rawViewTopPixel = view.rawViewTopPixel
      }

      override def notify(pub: Tab#Pub, event: TabEvent): Unit = {
        event match {
          case TabClosedEvent =>
            closeTab(pub)

          case TabActivatedEvent =>
            currentTabHandler.foreach(_.saveRawViewTopPixel())
            view.title = appName + " - " + title
            view.setRawViewTexts(offsetViewText, numericViewText)
            view.scrollRawView(rawViewTopPixel)
            currentTabHandler = Some(this)
        }
      }
    }

    view.title = appName
    view.numericViewWidth = MainPresenter.DefaultBytesPerRow * 3 - 1
    view.disableCommand(MainView.Command.Close)
    view.show()

    view.subscribe(ViewHandler)
  }
}
