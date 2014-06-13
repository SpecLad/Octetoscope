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

package ru.corrigendum.octetoscope.core

import ru.corrigendum.octetoscope.abstractinfra.Blob

trait MoleculeBuilderDissector[Value] extends MoleculeDissectorC[Value] {
  final override def dissect(input: Blob, offset: InfoSize): MoleculeC[Value] = {
    val value = defaultValue
    val builder = new MoleculeBuilder()
    try {
      dissectMB(input, offset, builder, value)
    } catch {
      case trunc: MoleculeBuilderDissector.TruncatedException =>
        if (!builder.hasChildren) throw trunc.getCause
        builder.addNote(Quality.Broken, "truncated at \"%s\"".format(trunc.subPieceName))
    }
    builder.build(value)
  }

  def defaultValue: Value
  def dissectMB(input: Blob, offset: InfoSize, builder: MoleculeBuilder, value: Value)
}

object MoleculeBuilderDissector {
  class TruncatedException(cause: Throwable, val subPieceName: String) extends Exception(cause)
}

trait MoleculeBuilderUnitDissector extends MoleculeBuilderDissector[Unit] {
  final override def defaultValue = ()
  final override def dissectMB(input: Blob, offset: InfoSize, builder: MoleculeBuilder, value: Unit) {
    dissectMBU(input, offset, builder)
  }

  def dissectMBU(input: Blob, offset: InfoSize, builder: MoleculeBuilder)
}
