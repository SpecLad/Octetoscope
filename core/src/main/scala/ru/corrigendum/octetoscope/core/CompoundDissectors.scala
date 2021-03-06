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

import scala.collection.mutable

object CompoundDissectors {
  private class Array(size: Int, itemName: String, itemDissector: PlainDissector) extends MoleculeBuilderUnitDissector {
    private[this] val names = Array.tabulate(size)("%s #%d".format(itemName, _))

    override def dissectMBU(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder): Unit = {
      val add = new SequentialAdder(context, offset, builder)

      for (i <- 0 until size) add(names(i), itemDissector)
    }
  }

  def array(size: Int, itemName: String, itemDissector: PlainDissector): MoleculeBuilderUnitDissector =
    new Array(size, itemName, itemDissector)

  private class CollectingArray[V](
    size: Int, itemName: String, itemDissector: DissectorC[V], reprFuncMaybe: Option[Seq[V] => String]
  ) extends MoleculeBuilderDissector[IndexedSeq[V], mutable.ArrayBuffer[V]] {
    private[this] val names = Array.tabulate(size)("%s #%d".format(itemName, _))

    override def defaultWIP: mutable.ArrayBuffer[V] = mutable.ArrayBuffer[V]()

    override def postProcess(wip: mutable.ArrayBuffer[V]): IndexedSeq[V] = wip

    override def dissectMB(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder,
                           wip: mutable.ArrayBuffer[V]): Unit = {
      val add = new SequentialAdder(context, offset, builder)
      wip.sizeHint(size)

      for (i <- 0 until size) wip += add(names(i), itemDissector)

      reprFuncMaybe.foreach(f => builder.setReprLazy(f(wip)))
    }
  }

  def collectingArray[V](
    size: Int, itemName: String, itemDissector: DissectorC[V]
  ): MoleculeDissectorWithDefaultValueC[IndexedSeq[V]] =
    new CollectingArray[V](size, itemName, itemDissector, None)

  def collectingArray[V](
    size: Int, itemName: String, itemDissector: DissectorC[V], reprFunc: Seq[V] => String
  ): MoleculeDissectorWithDefaultValueC[IndexedSeq[V]] =
    new CollectingArray[V](size, itemName, itemDissector, Some(reprFunc))

  private class Sequence(itemName: String, itemDissector: PlainDissector) extends MoleculeBuilderUnitDissector {
    override def dissectMBU(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder): Unit = {
      val add = new SequentialAdder(context, offset, builder)
      while (!add.limitReached) add(itemName, itemDissector)
    }
  }

  def sequence(itemName: String, itemDissector: PlainDissector): MoleculeBuilderUnitDissector =
    new Sequence(itemName, itemDissector)

  private class Enum[V, E](underlying: DissectorCR[V], enumerators: Map[V, E]) extends DissectorCR[Option[E]] {
    override def dissect(context: DissectionContext, offset: InfoSize): AtomCR[Option[E]] = {
      val piece = underlying.dissect(context, offset)
      val contents = piece.contents

      enumerators.get(contents.value) match {
        case s: Some[E] =>
          Atom(piece.size, new ContentsR(s) {
            override def repr: String = contents.repr + " -> " + value.get.toString
          })
        case None =>
          Atom(piece.size, new ContentsR(None) {
            override def repr: String = contents.repr
          }, Seq(Note(NoteSeverity.Failure, "unknown enumerator")))
      }
    }
  }

  def enum[V, E](underlying: DissectorCR[V], enumerators: Map[V, E]): DissectorCR[Option[E]] =
    new Enum[V, E](underlying, enumerators)

  private val bitSbz = PrimitiveDissectors.bit +? CommonConstraints.`false`

  private type BitFieldWIP = (mutable.Builder[String, Set[String]], mutable.Builder[Long, Set[Long]])

  private class BitField(totalBits: Long,
                         namedBits: Map[Long, String],
                         sbz: Set[String],
                         unnamedReason: String,
                         unnamedConstraint: Constraint[Boolean]
                        ) extends MoleculeBuilderDissector[(Set[String], Set[Long]), BitFieldWIP] {
    private[this] val unnamedBit = PrimitiveDissectors.bit + unnamedConstraint

    override def defaultWIP: BitFieldWIP = (Set.newBuilder[String], Set.newBuilder[Long])
    override def postProcess(wip: BitFieldWIP): (Set[String], Set[Long]) =
      (wip._1.result(), wip._2.result())

    override def dissectMB(context: DissectionContext, offset: InfoSize,
                           builder: MoleculeBuilder,
                           wip: BitFieldWIP): Unit = {
      val add = new SequentialAdder(context, offset, builder)

      val setBitNames = IndexedSeq.newBuilder[String]

      for (i <- 0L until totalBits)
        namedBits.get(i) match {
          case Some(name) =>
            if (add(name, if (sbz(name)) bitSbz else PrimitiveDissectors.bit)) {
              setBitNames += name
              wip._1 += name
            }
          case None =>
            if (add("Bit #%d (%s)".format(i, unnamedReason), unnamedBit)) {
              setBitNames += "#" + i
              wip._2 += i
            }
        }

      builder.setRepr(setBitNames.result().mkString("<", " | ", ">"))
    }
  }

  def bitField(totalBits: Long,
               namedBits: Map[Long, String],
               sbz: Set[String] = Set.empty,
               unnamedReason: String = "unknown",
               unnamedConstraint: Constraint[Boolean] = CommonConstraints.any
              ): MoleculeDissectorWithDefaultValueC[(Set[String], Set[Long])] =
    new BitField(totalBits, namedBits, sbz, unnamedReason, unnamedConstraint)
}
