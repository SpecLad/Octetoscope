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

import org.scalatest.FunSuite
import org.scalatest.MustMatchers._
import org.scalatest.Inside._
import org.scalatest.LoneElement._
import ru.corrigendum.octetoscope.core.PrimitiveDissectors._
import ru.corrigendum.octetoscope.abstractinfra.Blob

class AdderSuite extends FunSuite {
  test("sequential adder") {
    val blob = new ArrayBlob(Array[Byte](-1, 1, 0, 0, 0, 2, 0, 0, 0, -1))
    val builder = new MoleculeBuilder

    val adder = new SequentialAdder(blob, Bytes(1), builder)
    adder("alpha", sInt32L) mustBe 1
    adder("beta", sInt32L.asInstanceOf[DissectorO[Int]]) mustBe Some(2)

    builder.build() mustBe Molecule(Bytes(8), None, Seq(
      SubPiece("alpha", Bytes(0), Atom(Bytes(4), Some("1"))),
      SubPiece("beta", Bytes(4), Atom(Bytes(4), Some("2")))
    ))
  }

  def sequentialAdderThrowTest(invokeAdder: (SequentialAdder, Throwable) => Unit) {
    val adder = new SequentialAdder(Blob.empty, InfoSize(), new MoleculeBuilder)

    val cause = new IndexOutOfBoundsException
    val exc = the [MoleculeBuilderDissector.TruncatedException] thrownBy invokeAdder(adder, cause)
    exc.getCause must be theSameInstanceAs cause
    exc.subPieceName mustBe "alpha"
  }

  test("sequential adder - throw - DissectorO") {
    sequentialAdderThrowTest { (adder, cause) =>
      adder("alpha", new DissectorO[Unit] {
        override def dissectO(input: Blob, offset: InfoSize): (Piece, Option[Unit]) = throw cause
      })
    }
  }

  test("sequential adder - throw - Dissector") {
    sequentialAdderThrowTest { (adder, cause) =>
      adder("alpha", new Dissector[Unit] {
        override def dissect(input: Blob, offset: InfoSize): (Piece, Unit) = throw cause
      })
    }
  }

  test("random adder") {
    val builder = new MoleculeBuilder

    case class Value(var i: Int)

    val dissector = new MoleculeBuilderDissector[Value] {
      def defaultValue: Value = Value(0)
      def dissectMB(input: Blob, offset: InfoSize, builder: MoleculeBuilder, value: Value) {
        value.i = 1
        offset mustBe Bytes(3)
        builder.addChild("alpha", Bytes(0), Atom(Bytes(1), None))
      }
    }

    val adder = new RandomAdder(Blob.empty, Bytes(1), builder)
    adder("omega", Bytes(2), dissector) mustBe Value(1)

    builder.build() mustBe Molecule(Bytes(3), None, Seq(
      SubPiece("omega", Bytes(2), Molecule(Bytes(1), None, Seq(
        SubPiece("alpha", Bytes(0), Atom(Bytes(1), None))
      )))
    ))
  }

  test("random adder - throw") {
    val builder = new MoleculeBuilder

    case class Value(var i: Int)

    val dissector = new MoleculeBuilderDissector[Value] {
      def defaultValue: Value = Value(0)
      def dissectMB(input: Blob, offset: InfoSize, builder: MoleculeBuilder, value: Value) {
        value.i = 1
        throw new MoleculeBuilderDissector.TruncatedException(new IndexOutOfBoundsException, "alpha")
      }
    }

    val adder = new RandomAdder(Blob.empty, Bytes(1), builder)
    adder("omega", Bytes(2), dissector) mustBe Value(0)

    inside(builder.build()) { case Molecule(size_, _, children, notes) =>
      size_ mustBe InfoSize()
      children mustBe empty
      notes.loneElement.quality mustBe PieceQuality.Broken
      notes.loneElement.text must include ("\"omega\"")
    }
  }
}
