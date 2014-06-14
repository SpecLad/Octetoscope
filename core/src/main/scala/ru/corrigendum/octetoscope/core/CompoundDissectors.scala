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

import scala.collection.mutable
import ru.corrigendum.octetoscope.abstractinfra.Blob

object CompoundDissectors {
  private class Array(size: Int, itemName: String, itemDissector: PlainDissector) extends MoleculeBuilderUnitDissector {
    override def dissectMBU(input: Blob, offset: InfoSize, builder: MoleculeBuilder) {
      val add = new SequentialAdder(input, offset, builder)

      for (i <- 0 until size) add("%s #%d".format(itemName, i), itemDissector)
    }
  }

  def array(size: Int, itemName: String, itemDissector: PlainDissector): MoleculeBuilderUnitDissector =
    new Array(size, itemName, itemDissector)

  private class CollectingArray[V](
    size: Int, itemName: String, itemDissector: DissectorC[V], reprFuncMaybe: Option[Seq[V] => String]
  ) extends MoleculeBuilderDissector[mutable.Buffer[V]] {
    override def defaultWIP: mutable.Buffer[V] = new mutable.ArrayBuffer[V](size)
    override def dissectMB(input: Blob, offset: InfoSize, builder: MoleculeBuilder, value: mutable.Buffer[V]) {
      val add = new SequentialAdder(input, offset, builder)

      for (i <- 0 until size) value += add("%s #%d".format(itemName, i), itemDissector)

      reprFuncMaybe.foreach(f => builder.setReprLazy(f(value)))
    }
  }

  def collectingArray[V](
    size: Int, itemName: String, itemDissector: DissectorC[V]
  ): MoleculeBuilderDissector[mutable.Buffer[V]] =
    new CollectingArray[V](size, itemName, itemDissector, None)

  def collectingArray[V](
    size: Int, itemName: String, itemDissector: DissectorC[V], reprFunc: Seq[V] => String
  ): MoleculeBuilderDissector[mutable.Buffer[V]] =
    new CollectingArray[V](size, itemName, itemDissector, Some(reprFunc))

  private class Enum[V, E](underlying: DissectorCR[V], enumerators: Map[V, E]) extends DissectorCR[Option[E]] {
    override def dissect(input: Blob, offset: InfoSize): AtomCR[Option[E]] = {
      val piece = underlying.dissect(input, offset)
      val contents = piece.contents

      enumerators.get(contents.value) match {
        case Some(enumerator) =>
          Atom(piece.size, new ContentsR[Option[E]] {
            override def repr: String = contents.repr + " -> " + enumerator.toString
            override val value: Option[E] = Some(enumerator)
          })
        case None =>
          Atom(piece.size, new ContentsR[Option[E]] {
            override def repr: String = contents.repr
            override val value: Option[E] = None
          }, Seq(Note(Quality.Broken, "unknown enumerator")))
      }
    }
  }

  def enum[V, E](underlying: DissectorCR[V], enumerators: Map[V, E]): DissectorCR[Option[E]] =
    new Enum[V, E](underlying, enumerators)

  private val bitSbz = PrimitiveDissectors.bit +? CommonConstraints.`false`

  private class BitField(totalBits: Long,
                         namedBits: Map[Long, String],
                         sbz: Set[String],
                         unnamedReason: String)
      extends MoleculeBuilderDissector[mutable.Set[String]] {
    override def defaultWIP: mutable.Set[String] = mutable.Set()

    override def dissectMB(input: Blob, offset: InfoSize,
                           builder: MoleculeBuilder,
                           value: mutable.Set[String]) {
      val add = new SequentialAdder(input, offset, builder)

      val setBitNames = IndexedSeq.newBuilder[String]

      for (i <- 0L until totalBits)
        namedBits.get(i) match {
          case Some(name) =>
            if (add(name, if (sbz(name)) bitSbz else PrimitiveDissectors.bit)) {
              setBitNames += name
              value += name
            }
          case None =>
            if (add("Bit #%d (%s)".format(i, unnamedReason), PrimitiveDissectors.bit))
              setBitNames += "#" + i
        }

      builder.setRepr(setBitNames.result().mkString("<", " | ", ">"))
    }
  }

  def bitField(totalBits: Long,
               namedBits: Map[Long, String],
               sbz: Set[String] = Set.empty,
               unnamedReason: String = "unknown"): MoleculeDissectorC[collection.Set[String]] =
    new BitField(totalBits, namedBits, sbz, unnamedReason)
}
