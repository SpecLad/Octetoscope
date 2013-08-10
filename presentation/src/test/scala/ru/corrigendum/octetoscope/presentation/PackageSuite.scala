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

package ru.corrigendum.octetoscope.presentation

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers._
import ru.corrigendum.octetoscope.core.{NamedPiece, Molecule, Atom, VersionInfo}
import ru.corrigendum.octetoscope.abstractui.DisplayTreeNode

class PackageSuite extends FunSuite {
  test("presentVersionInfo") {
    val hash = "1234" * 10
    presentVersionInfo(VersionInfo("1.2", 0, hash, dirty = false)) must equal ("1.2-g1234123")
    presentVersionInfo(VersionInfo("1.2", 34, hash, dirty = false)) must equal ("1.2+34-g1234123")
    presentVersionInfo(VersionInfo("1.2", 0, hash, dirty = true)) must equal ("1.2-g1234123-dirty")
    presentVersionInfo(VersionInfo("1.2", 34, hash, dirty = true)) must equal ("1.2+34-g1234123-dirty")
  }

  test("presentPiece - atom") {
    presentPiece(Atom("alpha")) must equal (DisplayTreeNode("WHOLE: alpha", Nil))
  }

  test("presentPiece - molecule") {
    val molecule =
      Molecule("beta", Seq(
        NamedPiece("one", Atom("gamma")),
        NamedPiece("two", Atom("delta"))))

    val displayed =
      DisplayTreeNode("WHOLE: beta", Seq(
        DisplayTreeNode("one: gamma", Nil),
        DisplayTreeNode("two: delta", Nil)
      ))

    presentPiece(molecule) must equal (displayed)
  }
}
