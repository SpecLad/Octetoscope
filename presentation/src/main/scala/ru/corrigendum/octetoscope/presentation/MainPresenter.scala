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

import ru.corrigendum.octetoscope.abstractui.MainView
import ru.corrigendum.octetoscope.abstractui.MainView._
import ru.corrigendum.octetoscope.core.{DetectionFailedException, TooSmallToDissectException, DissectorDriver, VersionInfo}
import ru.corrigendum.octetoscope.abstractui.MainView.CommandEvent
import java.io.IOException

class MainPresenter(strings: PresentationStrings,
                    appName: String,
                    view: MainView,
                    boxer: DialogBoxer,
                    dissectorDriver: DissectorDriver) {
  view.title = appName
  view.show()

  view.subscribe(viewHandler)

  var numTabs = 0

  private def closeTab(tab: MainView.Tab) {
    tab.close()
    numTabs -= 1
    if (numTabs == 0) view.title = appName
  }

  private object viewHandler extends MainView#Sub {
    override def notify(pub: MainView#Pub, event: Event) {
      event match {
        case ClosedEvent => pub.dispose()

        case CommandEvent(MainView.Command.About) =>
          boxer.showMessageBox(strings.appVersionString(appName, presentVersionInfo(VersionInfo.ours)))

        case CommandEvent(MainView.Command.Quit) => pub.dispose()

        case CommandEvent(MainView.Command.Open) =>
          pub.showFileOpenBox() match {
            case None =>
            case Some(path) =>
              val piece = try {
                dissectorDriver(path)
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

              pub.addTab(path.getName, path.toString, presentPiece(piece)).subscribe(new TabHandler(path.getName))
              numTabs += 1
              view.title = appName + " - " + path.getName
          }

        case CommandEvent(MainView.Command.Close) =>
          closeTab(pub.activeTab.get)

        case CommandEvent(_) => // workaround for bug SI-7206
      }
    }
  }

  private class TabHandler(title: String) extends MainView.Tab#Sub {
    override def notify(pub: Tab#Pub, event: TabEvent) {
      event match {
        case TabClosedEvent =>
          closeTab(pub)

        case TabActivatedEvent =>
          view.title = appName + " - " + title
      }
    }
  }
}
