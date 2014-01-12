/*
  This file is part of Octetoscope.
  Copyright (C) 2013 Octetoscope contributors (see /AUTHORS.txt)

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
import org.scalatest.Matchers._
import ru.corrigendum.octetoscope.core.PrimitiveDissectors._
import ru.corrigendum.octetoscope.abstractinfra.Blob

class AdderSuite extends FunSuite {
  test("sequential adder") {
    val blob = new ArrayBlob(Array[Byte](-1, 1, 0, 0, 0, 2, 0, 0, 0, -1))
    val builder = new MoleculeBuilder

    val adder = new SequentialAdder(blob, Bytes(1), builder)
    adder("alpha", sInt32L) shouldBe 1
    adder("beta", sInt32L.asInstanceOf[DissectorO[Int]]) shouldBe Some(2)

    builder.build() shouldBe Molecule(Bytes(8), None, Seq(
      SubPiece("alpha", Bytes(0), Atom(Bytes(4), Some("1"))),
      SubPiece("beta", Bytes(4), Atom(Bytes(4), Some("2")))
    ))
  }

  def sequentialAdderThrowTest(invokeAdder: SequentialAdder => Unit) {
    val builder = new MoleculeBuilder
    val adder = new SequentialAdder(Blob.empty, InfoSize(), builder)

    val exc = the [MoleculeBuilderDissector.Stop] thrownBy invokeAdder(adder)
    exc.getCause shouldBe an [IndexOutOfBoundsException]

    val result = builder.build()
    result.quality shouldBe PieceQuality.Broken
    result.notes should have size 1
    result.notes.head should include ("\"alpha\"")
  }

  test("sequential adder - throw - DissectorO") {
    sequentialAdderThrowTest { adder =>
      adder("alpha", new DissectorO[Unit] {
        override def dissectO(input: Blob, offset: InfoSize): (Piece, Option[Unit]) = throw new IndexOutOfBoundsException
      })
    }
  }

  test("sequential adder - throw - Dissector") {
    sequentialAdderThrowTest { adder =>
      adder("alpha", new Dissector[Unit] {
        override def dissect(input: Blob, offset: InfoSize): (Piece, Unit) = throw new IndexOutOfBoundsException
      })
    }
  }

  test("random adder") {
    val blob = new ArrayBlob(Array[Byte](-1, 3, 0, 0, 0, -1, 4, 0, 0, 0, -1))
    val builder = new MoleculeBuilder

    val adder = new RandomAdder(blob, Bytes(1), builder)
    adder("alpha", Bytes(0), sInt32L) shouldBe 3
    adder("beta", Bytes(5), sInt32L.asInstanceOf[DissectorO[Int]]) shouldBe Some(4)

    builder.build() shouldBe Molecule(Bytes(9), None, Seq(
      SubPiece("alpha", Bytes(0), Atom(Bytes(4), Some("3"))),
      SubPiece("beta", Bytes(5), Atom(Bytes(4), Some("4")))
    ))
  }
}
