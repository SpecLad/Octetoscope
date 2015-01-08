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

import org.scalatest.FunSuite
import org.scalatest.LoneElement._
import org.scalatest.MustMatchers._
import ru.corrigendum.octetoscope.abstractinfra.Blob

class DissectorSuite extends FunSuite {
  test("MoleculeBuilderPostProcessingDissector.defaultValue") {
    val mbd = new MoleculeBuilderPostProcessingDissector[String, Int] {
      override def defaultWIP: Int = 1
      override def postProcess(wip: Int): String = wip.toString
      override def dissectMB(input: Blob, offset: InfoSize, builder: MoleculeBuilder, value: Int) { }
    }

    mbd.defaultValue mustBe "1"
  }

  test("MoleculeBuilderPostProcessingDissector.dissect") {
    case class WIP(var i: Int)

    val child = Atom(Bytes(1), new ToStringContents("a"))

    val mbd = new MoleculeBuilderPostProcessingDissector[String, WIP] {
      override def defaultWIP: WIP = WIP(1)
      override def postProcess(wip: WIP): String = wip.i.toString
      override def dissectMB(input: Blob, offset: InfoSize, builder: MoleculeBuilder, value: WIP) {
        value.i mustBe 1
        value.i = 2
        builder.addChild("alpha", Bytes(1), child)
      }
    }

    mbd.dissect(Blob.empty) mustBe
      Molecule(Bytes(2), new EagerContents("2"), Seq(
        SubPiece("alpha", Bytes(1), child)
      ))
  }

  test("MoleculeBuilderPostProcessingDissector - truncated - empty") {
    val cause = new IndexOutOfBoundsException

    val truncated = new MoleculeBuilderPostProcessingDissector[Unit, Unit] {
      override def defaultWIP = ()
      override def postProcess(wip: Unit): Unit = wip
      override def dissectMB(input: Blob, offset: InfoSize, builder: MoleculeBuilder, value: Unit) {
        throw new MoleculeBuilderDissector.TruncatedException(cause, "alpha")
      }
    }

    val exc = the [IndexOutOfBoundsException] thrownBy truncated.dissect(Blob.empty)
    exc must be theSameInstanceAs cause
  }

  test("MoleculeBuilderPostProcessingDissector - truncated - non-empty") {
    case class WIP(var i: Int)

    val child = Atom(Bytes(1), EmptyContents)

    val truncated = new MoleculeBuilderPostProcessingDissector[String, WIP] {
      override def defaultWIP = WIP(0)
      override def postProcess(wip: WIP): String = wip.i.toString
      override def dissectMB(input: Blob, offset: InfoSize, builder: MoleculeBuilder, value: WIP) {
        value.i = 1
        builder.addChild("alpha", InfoSize(), child)
        throw new MoleculeBuilderDissector.TruncatedException(new IndexOutOfBoundsException, "beta")
      }
    }

    val molecule = truncated.dissect(Blob.empty)
    molecule.children mustBe Seq(SubPiece("alpha", InfoSize(), child))
    molecule.notes.loneElement.pieceQuality mustBe Quality.Broken
    molecule.notes.loneElement.text must include ("\"beta\"")
    molecule.contents.value mustBe "1"
  }

  test("MoleculeBuilderUnitDissector.dissectMB") {
    val child = Atom(Bytes(1), new ToStringContents("a"))

    val mbud = new MoleculeBuilderUnitDissector {
      override def dissectMBU(input: Blob, offset: InfoSize, builder: MoleculeBuilder) {
        builder.addChild("alpha", Bytes(1), child)
      }
    }

    val builder = new MoleculeBuilder
    mbud.dissectMB(Blob.empty, InfoSize(), builder, ())

    builder.build(()) mustBe
      Molecule(Bytes(2), EmptyContents, Seq(
        SubPiece("alpha", Bytes(1), child)
      ))
  }
}
