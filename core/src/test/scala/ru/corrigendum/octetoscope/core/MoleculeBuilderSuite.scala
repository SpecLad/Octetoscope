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

import org.scalatest.{BeforeAndAfter, FunSuite}
import org.scalatest.MustMatchers._

class MoleculeBuilderSuite extends FunSuite with BeforeAndAfter {
  var builder: MoleculeBuilder[Int] = _

  before {
    builder = new MoleculeBuilder(5)
  }

  test("default") {
    builder.hasChildren mustBe false
    builder.build() mustBe Molecule(InfoSize(), new EagerContents(5), Seq())
  }

  test("with repr") {
    builder.setRepr("value")
    builder.build() mustBe Molecule(InfoSize(), new EagerContents(5, Some("value")), Seq())
  }

  test("with lazy repr") {
    builder.setReprLazy("value")
    builder.build() mustBe Molecule(InfoSize(), new EagerContents(5, Some("value")), Seq())
  }

  test("with children") {
    val alpha = SubPiece("alpha", Bytes(0), Atom(Bytes(1), new EagerContents((), Some("a"))))
    val beta = SubPiece("beta", Bytes(2), Atom(Bytes(2), EmptyContents))
    val gamma = SubPiece("gamma", Bytes(1), Atom(Bytes(1), EmptyContents))

    builder.addChild(alpha.name, alpha.offset, alpha.piece)
    builder.hasChildren mustBe true

    builder.addChild(beta.name, beta.offset, beta.piece)
    builder.addChild(gamma.name, gamma.offset, gamma.piece)

    builder.build() mustBe Molecule(Bytes(4), new EagerContents(5), Seq(alpha, beta, gamma))
  }

  test("with notes") {
    builder.addNote(Quality.Dubious, "foo")
    builder.addNote(Quality.Bad, "bar")
    builder.build() mustBe Molecule(InfoSize(), new EagerContents(5), Seq(), notes = Seq(
      Note(Quality.Dubious, "foo"),
      Note(Quality.Bad, "bar"))
    )
  }

  test("fixed size") {
    builder.addChild("alpha", Bytes(1), Atom(Bytes(5), EmptyContents))
    builder.fixSize(Bytes(10))
    builder.addChild("beta", Bytes(8), Atom(Bytes(5), EmptyContents))
    builder.build().size mustBe Bytes(10)
  }
}
