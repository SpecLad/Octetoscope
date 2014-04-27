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

package ru.corrigendum.octetoscope

import ru.corrigendum.octetoscope.core._
import ru.corrigendum.octetoscope.abstractui.DisplayTreeNode
import java.awt.Color

package object presentation {
  private[presentation] val QualityColors = Map(
    Quality.Good -> Color.WHITE,
    Quality.Dubious -> Color.YELLOW,
    Quality.Bad -> Color.PINK,
    Quality.Broken -> new Color(255, 200, 255)
  )

  private[presentation] def presentVersionInfo(vi: VersionInfo): String = {
    "%s%s-g%s%s".format(
      vi.releaseVersion,
      if (vi.extraCommits != 0) "+" + vi.extraCommits else "",
      vi.commitHash.substring(0, 7),
      if (vi.dirty) "-dirty" else ""
    )
  }

  private[presentation] def presentPiece(piece: PlainPiece): DisplayTreeNode = {
    def helper(np: SubPiece): DisplayTreeNode = {
      val displayText = StringBuilder.newBuilder
      displayText ++= np.name

      for (repr <- np.piece.contents.reprO)
        displayText ++= ": " ++= repr

      DisplayTreeNode(
        displayText.result(),
        np.piece.notes.map(n => (QualityColors(n.pieceQuality), n.text)),
        np.piece match {
          case _: PlainAtom => None
          case m: PlainMolecule => Some(() => m.children.map(helper))
        }
      )
    }

    helper(SubPiece("WHOLE", InfoSize(), piece))
  }
}
