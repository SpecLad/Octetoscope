/*
  This file is part of Octetoscope.
  Copyright (C) 2013-2016 Octetoscope contributors (see /AUTHORS.txt)

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

package ru.corrigendum.octetoscope.core

import ru.corrigendum.octetoscope.abstractinfra.Blob

sealed case class DissectionContext(input: Blob,
                                    softLimit: InfoSize,
                                    untested: UntestedCallback = DissectionContext.ignoreUntested) {
  require(softLimit <= Bytes(input.size))
}

object DissectionContext {
  val ignoreUntested: UntestedCallback = () => ()

  def apply(): DissectionContext = DissectionContext(Blob.empty, InfoSize())
  def apply(input: Blob): DissectionContext = DissectionContext(input, Bytes(input.size))
}

trait Dissector[+V, +C <: Contents[V]] {
  def dissect(context: DissectionContext, offset: InfoSize = InfoSize()): Piece[C]

  def +?(constraint: Constraint[V]): Dissector[V, C] =
    SpecialDissectors.constrained(this, constraint, NoteSeverity.Warning)
  def +(constraint: Constraint[V]): Dissector[V, C] =
    SpecialDissectors.constrained(this, constraint, NoteSeverity.Error)
}

trait MoleculeDissector[+V, +C <: Contents[V]] extends Dissector[V, C] {
  override def dissect(context: DissectionContext, offset: InfoSize = InfoSize()): Molecule[C]
}

trait DissectorWithDefaultValue[+V, +C <: Contents[V]] extends Dissector[V, C] {
  def defaultValue: V
}
