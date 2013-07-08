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
import ru.corrigendum.octetoscope.abstractui.MainView.{CommandEvent, ClosedEvent, Event}
import ru.corrigendum.octetoscope.core.VersionInfo

class MainPresenter(strings: PresentationStrings, appName: String, view: MainView, boxer: DialogBoxer) {
  view.title = appName
  view.show()

  view.subscribe(new view.Sub {
    def notify(pub: view.Pub, event: Event) {
      event match {
        case ClosedEvent() => pub.dispose()
        case CommandEvent(MainView.Command.About) =>
          boxer.showMessageBox(pub, strings.appVersionString(appName, formatVersionInfo(VersionInfo.ours)))
        case CommandEvent(MainView.Command.Quit) => pub.dispose()
        case CommandEvent(MainView.Command.Open) => {
          pub.showFileOpenBox() match {
            case None =>
            case Some(path) => pub.addTab(path.getName, path.toString)
          }
        }
        case CommandEvent(_) => // workaround for bug SI-7206
      }
    }
  })
}
