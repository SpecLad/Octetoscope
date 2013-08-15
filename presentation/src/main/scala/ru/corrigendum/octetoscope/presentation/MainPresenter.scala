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

import ru.corrigendum.octetoscope.abstractui.MainView
import ru.corrigendum.octetoscope.abstractui.MainView._
import ru.corrigendum.octetoscope.core.{DissectorDriver, VersionInfo}
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

  private object viewHandler extends MainView#Sub {
    override def notify(pub: MainView#Pub, event: Event) {
      event match {
        case ClosedEvent => pub.dispose()

        case CommandEvent(MainView.Command.About) =>
          boxer.showMessageBox(strings.appVersionString(appName, presentVersionInfo(VersionInfo.ours)))

        case CommandEvent(MainView.Command.Quit) => pub.dispose()

        case CommandEvent(MainView.Command.Open) => {
          pub.showFileOpenBox() match {
            case None =>
            case Some(path) => {
              val piece = try {
                dissectorDriver.dissect(path)
              } catch {
                case ioe: IOException => {
                  boxer.showMessageBox(strings.errorReadingFile(ioe.getMessage))
                  return
                }
              }

              pub.addTab(path.getName, path.toString, presentPiece(piece)).subscribe(tabHandler)
            }
          }
        }

        case CommandEvent(_) => // workaround for bug SI-7206
      }
    }
  }

  private object tabHandler extends MainView.Tab#Sub {
    override def notify(pub: Tab#Pub, event: TabEvent) {
      event match {
        case TabClosedEvent => pub.close()
      }
    }
  }
}
