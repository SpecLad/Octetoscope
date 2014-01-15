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

import org.scalatest.{BeforeAndAfter, FunSuite}
import org.scalatest.Matchers._

class MoleculeBuilderSuite extends FunSuite with BeforeAndAfter {
  var builder: MoleculeBuilder = _

  before {
    builder = new MoleculeBuilder
  }

  test("default") {
    builder.hasChildren shouldBe false
    builder.build() shouldBe Molecule(InfoSize(), None, Seq())
  }

  test("with repr") {
    builder.setRepr("value")
    builder.build() shouldBe Molecule(InfoSize(), Some("value"), Seq())
  }

  test("with children") {
    val alpha = SubPiece("alpha", Bytes(0), Atom(Bytes(1), Some("a")))
    val beta = SubPiece("beta", Bytes(2), Atom(Bytes(2), None))
    val gamma = SubPiece("gamma", Bytes(1), Atom(Bytes(1), None))

    builder.addChild(alpha.name, alpha.offset, alpha.piece)
    builder.hasChildren shouldBe true

    builder.addChild(beta.name, beta.offset, beta.piece)
    builder.addChild(gamma.name, gamma.offset, gamma.piece)

    builder.build() shouldBe Molecule(Bytes(4), None, Seq(alpha, beta, gamma))
  }

  test("with quality") {
    builder.impair(PieceQuality.Bad)
    builder.build() shouldBe Molecule(InfoSize(), None, Seq(), PieceQuality.Bad)
  }

  test("with decreasing quality") {
    builder.impair(PieceQuality.Dubious)
    builder.impair(PieceQuality.Broken)
    builder.build() shouldBe Molecule(InfoSize(), None, Seq(), PieceQuality.Broken)
  }

  test("with increasing quality") {
    builder.impair(PieceQuality.Broken)
    builder.impair(PieceQuality.Dubious)
    builder.build() shouldBe Molecule(InfoSize(), None, Seq(), PieceQuality.Broken)
  }

  test("with notes") {
    builder.addNote("foo")
    builder.addNote("bar")
    builder.build() shouldBe Molecule(InfoSize(), None, Seq(), notes = Seq("foo", "bar"))
  }
}
