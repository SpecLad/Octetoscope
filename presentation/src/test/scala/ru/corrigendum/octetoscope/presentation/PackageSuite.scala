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
import ru.corrigendum.octetoscope.core._
import ru.corrigendum.octetoscope.abstractui.DisplayTreeNode

class PackageSuite extends FunSuite {
  test("presentVersionInfo") {
    val hash = "1234" * 10
    presentVersionInfo(VersionInfo("1.2", 0, hash, dirty = false)) must equal ("1.2-g1234123")
    presentVersionInfo(VersionInfo("1.2", 34, hash, dirty = false)) must equal ("1.2+34-g1234123")
    presentVersionInfo(VersionInfo("1.2", 0, hash, dirty = true)) must equal ("1.2-g1234123-dirty")
    presentVersionInfo(VersionInfo("1.2", 34, hash, dirty = true)) must equal ("1.2+34-g1234123-dirty")
  }

  test("presentPiece - atom - with value") {
    presentPiece(Atom(40, Some("alpha"))) must equal (DisplayTreeNode("WHOLE: alpha", Nil))
  }

  test("presentPiece - atom - without value") {
    presentPiece(Atom(16, None)) must equal (DisplayTreeNode("WHOLE", Nil))
  }

  test("presentPiece - molecule") {
    val molecule =
      Molecule(100, Some("beta"), Seq(
        SubPiece("one", Offset(0), Atom(10, Some("gamma"))),
        SubPiece("two", Offset(50), Atom(10, None))))

    val displayed =
      DisplayTreeNode("WHOLE: beta", Seq(
        DisplayTreeNode("one: gamma", Nil),
        DisplayTreeNode("two", Nil)
      ))

    presentPiece(molecule) must equal (displayed)
  }
}
