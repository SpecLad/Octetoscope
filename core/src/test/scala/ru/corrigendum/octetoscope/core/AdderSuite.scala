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
import org.scalatest.Inside._
import org.scalatest.LoneElement._
import org.scalatest.MustMatchers._
import ru.corrigendum.octetoscope.core.CommonConstraints._
import ru.corrigendum.octetoscope.core.PrimitiveDissectors._

class AdderSuite extends FunSuite {
  test("sequential adder") {
    val blob = new ArrayBlob(Array[Byte](-1, 1, 0, 0, 0, 2, 0, 0, 0, -1))
    val builder = new MoleculeBuilder

    val adder = new SequentialAdder(DissectionContext(blob), Bytes(1), builder)
    adder("alpha", sInt32L) mustBe 1
    adder.getContents("beta", sInt32L) mustBe new ToStringContents(2)

    builder.build(()) mustBe Molecule(Bytes(8), EmptyContents, Seq(
      SubPiece("alpha", Bytes(0), Atom(Bytes(4), new ToStringContents[Int](1))),
      SubPiece("beta", Bytes(4), Atom(Bytes(4), new ToStringContents[Int](2)))
    ))
  }

  def sequentialAdderThrowTest(invokeAdder: (SequentialAdder, Throwable) => Unit): Unit = {
    val adder = new SequentialAdder(DissectionContext(), InfoSize(), new MoleculeBuilder)

    val cause = new IndexOutOfBoundsException
    val exc = the [MoleculeBuilderDissector.TruncatedException] thrownBy invokeAdder(adder, cause)
    exc.getCause must be theSameInstanceAs cause
    exc.subPieceName mustBe "alpha"
  }

  test("sequential adder - throw") {
    sequentialAdderThrowTest { (adder, cause) =>
      adder("alpha", new DissectorC[Unit] {
        override def dissect(context: DissectionContext, offset: InfoSize) = throw cause
      })
    }
  }

  test("sequential adder - filtered") {
    val blob = new ArrayBlob(Array[Byte](1, 2, 3))
    val builder = new MoleculeBuilder

    val adder = new SequentialAdder(DissectionContext(blob), Bytes(0), builder)
    val c1 = lessThan(2.toByte, "two")
    val c2 = lessThan(3.toByte, "three")

    adder.filtered("alpha", sInt8)(c1, c2) mustBe Some(1)
    adder.filtered("beta", sInt8)(c1, c2) mustBe None
    adder.filtered("gamma", sInt8)(c1, c2) mustBe None

    val molecule = builder.build(())

    molecule.children(0).piece.notes mustBe Nil
    molecule.children(1).piece.notes mustBe Seq(Note(NoteSeverity.Error, c1.note(NoteSeverity.Error)))
    molecule.children(2).piece.notes mustBe Seq(
      Note(NoteSeverity.Error, c1.note(NoteSeverity.Error)), Note(NoteSeverity.Error, c2.note(NoteSeverity.Error)))
  }

  test("sequential adder - limitReached") {
    val blob = new ArrayBlob(Array[Byte](1, 2, 3, 4))
    val builder = new MoleculeBuilder

    val adder = new SequentialAdder(DissectionContext(blob, Bytes(3)), Bytes(1), builder)
    adder.limitReached mustBe false

    adder("alpha", sInt8)
    adder.limitReached mustBe false

    adder("beta", sInt8)
    adder.limitReached mustBe true

    adder("gamma", sInt8)
    adder.limitReached mustBe true
  }

  test("random adder") {
    val builder = new MoleculeBuilder

    val dissector = new DissectorWithDefaultValueC[Int] {
      override def defaultValue: Int = 1
      override def dissect(context: DissectionContext, offset: InfoSize): Piece[Contents[Int]] = {
        offset mustBe Bytes(3)
        Atom(Bytes(3), new EagerContents(2))
      }
    }

    val adder = new RandomAdder(DissectionContext(), Bytes(1), builder)
    adder("omega", Bytes(2), dissector) mustBe 2

    builder.build(()) mustBe Molecule(Bytes(5), EmptyContents, Seq(
      SubPiece("omega", Bytes(2), Atom(Bytes(3), new EagerContents(2)))))
  }

  test("random adder - throw") {
    val builder = new MoleculeBuilder

    val dissector = new DissectorWithDefaultValueC[Int] {
      override def defaultValue: Int = 1
      override def dissect(context: DissectionContext, offset: InfoSize): Piece[Contents[Int]] =
        throw new IndexOutOfBoundsException
    }

    val adder = new RandomAdder(DissectionContext(), Bytes(1), builder)
    adder("omega", Bytes(2), dissector) mustBe 1

    inside(builder.build(())) { case Molecule(size_, _, children, notes) =>
      size_ mustBe InfoSize()
      children mustBe empty
      notes.loneElement.severity mustBe NoteSeverity.Failure
      notes.loneElement.text must include ("\"omega\"")
    }
  }
}
