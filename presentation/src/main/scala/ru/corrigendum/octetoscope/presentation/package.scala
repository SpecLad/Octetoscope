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

package ru.corrigendum.octetoscope

import ru.corrigendum.octetoscope.core._
import ru.corrigendum.octetoscope.abstractui.DisplayTreeNode

package object presentation {
  private[presentation] def presentVersionInfo(vi: VersionInfo): String = {
    "%s%s-g%s%s".format(
      vi.releaseVersion,
      if (vi.extraCommits != 0) "+" + vi.extraCommits else "",
      vi.commitHash.substring(0, 7),
      if (vi.dirty) "-dirty" else ""
    )
  }

  private[presentation] def presentPiece(piece: Piece): DisplayTreeNode = {
    def helper(np: SubPiece): DisplayTreeNode = {
      val displayText = np.piece.repr match {
        case None => np.name
        case Some(repr) => np.name + ": " + repr
      }

      DisplayTreeNode(
        displayText,
        np.piece match {
          case _: Atom => Nil
          case m: Molecule => m.children.map(helper)
        }
      )
    }

    helper(SubPiece("WHOLE", Offset(), piece))
  }
}
