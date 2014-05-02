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

package ru.corrigendum.octetoscope.application

import ru.corrigendum.octetoscope.swingui.SwingApplication
import ru.corrigendum.octetoscope.presentation.{PresentationStrings, DialogBoxerImpl, MainPresenter}
import ru.corrigendum.octetoscope.infra.{DefaultBinaryReader, MessageLocalizer}
import ru.corrigendum.octetoscope.abstractui.UIStrings
import ru.corrigendum.octetoscope.core.getDetector
import ru.corrigendum.octetoscope.core.getDissectorDriver
import ru.corrigendum.octetoscope.dissectors.magicMap

object Octetoscope extends App {
  private val APPLICATION_NAME = "Octetoscope"

  if (args.length != 0) {
    Console.err.println("Usage: octetoscope")
    sys.exit(1)
  }

  val uiStrings = MessageLocalizer.localize[UIStrings](classOf[UIStrings], UIStrings.translationMap)
  val presentationStrings = MessageLocalizer.localize[PresentationStrings](
    classOf[PresentationStrings], PresentationStrings.translationMap)

  SwingApplication.setStyle()
  SwingApplication.start(uiStrings,
    view => new MainPresenter(
      presentationStrings, APPLICATION_NAME, view,
      new DialogBoxerImpl(view, APPLICATION_NAME),
      getDissectorDriver(DefaultBinaryReader, getDetector(magicMap))
    ))
}
