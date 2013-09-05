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
import org.scalatest.matchers.MustMatchers._

class MoleculeBuilderSuite extends FunSuite with BeforeAndAfter {
  var builder: MoleculeBuilder = _

  before {
    builder = new MoleculeBuilder
  }

  test("default") {
    builder.build() must equal (Molecule(0, None, Seq()))
  }

  test("with repr") {
    builder.setRepr("value")
    builder.build() must equal (Molecule(0, Some("value"), Seq()))
  }

  test("with children") {
    val alpha = SubPiece("alpha", Bytes(0), Atom(8, Some("a")))
    val beta = SubPiece("beta", Bytes(2), Atom(16, None))
    val gamma = SubPiece("gamma", Bytes(1), Atom(8, None))

    builder.addChild(alpha.name, alpha.offset, alpha.piece)
    builder.addChild(beta.name, beta.offset, beta.piece)
    builder.addChild(gamma.name, gamma.offset, gamma.piece)

    builder.build() must equal (
      Molecule(32, None, Seq(alpha, beta, gamma))
    )
  }
}
