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
    builder.build() must equal (Molecule(None, Seq()))
  }

  test("with repr") {
    builder.setRepr("value")
    builder.build() must equal (Molecule(Some("value"), Seq()))
  }

  test("with children") {
    builder.addChild("alpha", Atom(Some("a")))
    builder.addChild("beta", Atom(None))
    builder.build() must equal (
      Molecule(None, Seq(
        NamedPiece("alpha", Atom(Some("a"))),
        NamedPiece("beta", Atom(None))
      ))
    )
  }
}
