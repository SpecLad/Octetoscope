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

import java.awt.Color

import ru.corrigendum.octetoscope.abstractinfra.Blob
import ru.corrigendum.octetoscope.abstractui.{DisplayTreeNodeEventListener, DisplayTreeNode}
import ru.corrigendum.octetoscope.core._

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

  private[presentation] def presentPiece(piece: PlainPiece,
                                         doubleClickHandler: (InfoSize, InfoSize) => Unit): DisplayTreeNode = {
    def helper(np: SubPiece, offset: InfoSize): DisplayTreeNode = {
      val displayText = StringBuilder.newBuilder
      displayText ++= np.name

      for (repr <- np.piece.contents.reprO)
        displayText ++= ": " ++= repr

      // We want the listener to only capture one value, not the whole np object.
      val size = np.piece.size

      DisplayTreeNode(
        displayText.result(),
        np.piece.notes.map(n => (QualityColors(n.pieceQuality), n.text)),
        np.piece match {
          case _: PlainAtom => None
          case m: PlainMolecule => Some(() => m.children.map(c => helper(c, offset + c.offset)))
        },
        new DisplayTreeNodeEventListener {
          override def doubleClicked() { doubleClickHandler(offset, size) }
        }
      )
    }

    helper(SubPiece("WHOLE", InfoSize(), piece), InfoSize())
  }

  private val bytesAsHex = Array.tabulate(256)(_.formatted("%02x"))

  private[presentation] def presentBlobAsHexadecimal(blob: Blob, numColumns: Int): String = {
    require(numColumns > 0)
    blob.toArray.grouped(numColumns).map(_.map((b: Byte) => bytesAsHex(b & 0xff)).mkString(" ")).mkString("\n")
  }

  private[presentation] def generateBlobOffsets(blobSize: Long, numColumns: Int): String = {
    val format = if (blobSize <= 0xffL) "%02x"
                 else if (blobSize <= 0xffffL) "%04x"
                 else if (blobSize <= 0xffffffffL) "%08x"
                 else "%016x"
    (for (off <- 0L until blobSize by numColumns) yield format.format(off)).mkString("\n")
  }
}
