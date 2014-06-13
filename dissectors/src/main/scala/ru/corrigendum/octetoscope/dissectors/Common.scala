/*
  This file is part of Octetoscope.
  Copyright (C) 2014 Octetoscope contributors (see /AUTHORS.txt)

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

package ru.corrigendum.octetoscope.dissectors

import ru.corrigendum.octetoscope.abstractinfra.Blob
import ru.corrigendum.octetoscope.core._

private object Common {
  private class Vector3[T](component: DissectorCR[T]) extends MoleculeBuilderDissector[Vector3.Value[T]] {
    import Vector3.Value

    override def defaultValue: Value[T] = Value(None, None, None)
    override def dissectMB(input: Blob, offset: InfoSize, builder: MoleculeBuilder, value: Value[T]) {
      val add = new SequentialAdder(input, offset, builder)
      val xc = add.getContents("x", component)
      value.x = Some(xc.value)
      val yc = add.getContents("y", component)
      value.y = Some(yc.value)
      val zc = add.getContents("z", component)
      value.z = Some(zc.value)

      builder.setReprLazy("(%s, %s, %s)".format(xc.repr, yc.repr, zc.repr))
    }
  }

  object Vector3 {
    case class Value[T](var x: Option[T], var y: Option[T], var z: Option[T])
  }

  def vector3[T](component: DissectorCR[T]): MoleculeDissectorC[Vector3.Value[T]] = new Vector3(component)
}
