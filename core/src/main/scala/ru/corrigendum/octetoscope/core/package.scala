/*
  This file is part of Octetoscope.
  Copyright (C) 2014-2016 Octetoscope contributors (see /AUTHORS.txt)

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

import ru.corrigendum.octetoscope.abstractinfra.Blob

package object core {
  type PieceC[V] = Piece[Contents[V]]
  type AtomC[V] = Atom[Contents[V]]
  type MoleculeC[V] = Molecule[Contents[V]]
  type DissectorC[V] = Dissector[V, Contents[V]]
  type MoleculeDissectorC[V] = MoleculeDissector[V, Contents[V]]
  type DissectorWithDefaultValueC[V] = DissectorWithDefaultValue[V, Contents[V]]
  type MoleculeDissectorWithDefaultValueC[V] = MoleculeDissectorC[V] with DissectorWithDefaultValueC[V]

  type PieceCR[V] = Piece[ContentsR[V]]
  type AtomCR[V] = Atom[ContentsR[V]]
  type MoleculeCR[V] = Molecule[ContentsR[V]]
  type DissectorCR[V] = Dissector[V, ContentsR[V]]
  type MoleculeDissectorCR[V] = MoleculeDissector[V, ContentsR[V]]

  type PlainPiece = PieceC[Any]
  type PlainAtom = AtomC[Any]
  type PlainMolecule = MoleculeC[Any]
  type PlainDissector = DissectorC[Any]
  type PlainMoleculeDissector = MoleculeDissectorC[Any]

  type Detector = Blob => Option[PlainDissector]

  type DissectorDriver = (Blob, UntestedCallback) => PlainPiece

  type UntestedCallback = () => Unit

  class TooSmallToDissectException(cause: Throwable) extends Exception(cause)
  class DetectionFailedException extends Exception

  def getDissectorDriver(detector: Detector): DissectorDriver =
    (blob: Blob, untested: UntestedCallback) =>
      try {
        val dissector = detector(blob).getOrElse(throw new DetectionFailedException)
        dissector.dissect(DissectionContext(blob, Bytes(blob.size), untested))
      } catch {
        // This should happen rarely, if ever, since most dissectors will return truncated
        // molecules instead of throwing.
        case iobe: IndexOutOfBoundsException => throw new TooSmallToDissectException(iobe)
      }

  def getDetector(magicMap: Seq[(Array[Byte], PlainDissector)]): Detector =
    (blob: Blob) => {
      magicMap.find {
        case (magic, _) =>
          magic.length <= blob.size && blob.getRangeAsArray(0, magic.length).sameElements(magic)
      }.map(_._2)
    }
}
