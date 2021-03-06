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

package ru.corrigendum.octetoscope.core

// useful as a WIP for dissectors returning simple values
case class Variable[T](var value: T)

trait MoleculeBuilderDissector[Value, WIP]
    extends MoleculeDissectorC[Value] with DissectorWithDefaultValueC[Value] {
  final override def defaultValue = postProcess(defaultWIP)
  final override def dissect(context: DissectionContext, offset: InfoSize): MoleculeC[Value] = {
    val value = defaultWIP
    val builder = new MoleculeBuilder()
    try {
      dissectMB(context, offset, builder, value)
    } catch {
      case trunc: MoleculeBuilderDissector.TruncatedException =>
        if (!builder.hasChildren) throw trunc.getCause
        builder.addNote(NoteSeverity.Failure, "truncated at \"%s\"".format(trunc.subPieceName))
    }
    builder.build(postProcess(value))
  }

  def defaultWIP: WIP
  def postProcess(wip: WIP): Value
  def dissectMB(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder, wip: WIP): Unit
}

object MoleculeBuilderDissector {
  class TruncatedException(cause: Throwable, val subPieceName: String) extends Exception(cause)
}

trait SimpleMoleculeBuilderDissector[Value] extends MoleculeBuilderDissector[Value, Value] {
  final override def postProcess(wip: Value): Value = wip
}

trait MoleculeBuilderUnitDissector extends SimpleMoleculeBuilderDissector[Unit] {
  final override def defaultWIP = ()
  final override def dissectMB(context: DissectionContext,
                               offset: InfoSize,
                               builder: MoleculeBuilder,
                               wip: Unit): Unit = {
    dissectMBU(context, offset, builder)
  }

  def dissectMBU(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder): Unit
}
