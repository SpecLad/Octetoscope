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

package ru.corrigendum.octetoscope.application

import ru.corrigendum.octetoscope.abstractui.UIStrings
import ru.corrigendum.octetoscope.core.{getDetector, getDissectorDriver}
import ru.corrigendum.octetoscope.dissectors.magicMap
import ru.corrigendum.octetoscope.infra.{DefaultBinaryReader, DefaultClock, MessageLocalizer}
import ru.corrigendum.octetoscope.presentation.{DialogBoxerImpl, LoggerImpl, MainPresenter, PresentationStrings}
import ru.corrigendum.octetoscope.swingui.SwingApplication

object Octetoscope extends App {
  {
    if (args.length != 0) {
      Console.err.println("Usage: octetoscope")
      sys.exit(1)
    }

    val applicationName = "Octetoscope"
    val uiStrings = MessageLocalizer.localize[UIStrings](classOf[UIStrings], UIStrings.translationMap)
    val presentationStrings = MessageLocalizer.localize[PresentationStrings](
      classOf[PresentationStrings], PresentationStrings.translationMap)

    SwingApplication.setStyle()
    SwingApplication.start(uiStrings,
      view => MainPresenter.attach(
        presentationStrings, applicationName, view,
        new DialogBoxerImpl(view, applicationName),
        new LoggerImpl(DefaultClock, view.logView),
        DefaultBinaryReader,
        DefaultClock,
        getDissectorDriver(getDetector(magicMap))
      ))
  }
}
