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

package ru.corrigendum.octetoscope.presentation

import org.scalatest.FunSuite
import org.scalatest.MustMatchers._
import org.scalatest.OptionValues._
import ru.corrigendum.octetoscope.core._
import ru.corrigendum.octetoscope.abstractui.DisplayTreeNode

class PackageSuite extends FunSuite {
  import PackageSuite._

  test("presentVersionInfo") {
    val hash = "1234" * 10
    presentVersionInfo(VersionInfo("1.2", 0, hash, dirty = false)) mustBe "1.2-g1234123"
    presentVersionInfo(VersionInfo("1.2", 34, hash, dirty = false)) mustBe "1.2+34-g1234123"
    presentVersionInfo(VersionInfo("1.2", 0, hash, dirty = true)) mustBe "1.2-g1234123-dirty"
    presentVersionInfo(VersionInfo("1.2", 34, hash, dirty = true)) mustBe "1.2+34-g1234123-dirty"
  }

  test("presentPiece - atom - with value") {
    presentPiece(Atom(Bytes(5), Some("alpha"))) mustBe DisplayTreeNode("WHOLE: alpha", GoodColor, None)
  }

  test("presentPiece - atom - without value") {
    presentPiece(Atom(Bytes(2), None)) mustBe DisplayTreeNode("WHOLE", GoodColor, None)
  }

  test("presentPiece - molecule") {
    val molecule =
      Molecule(Bytes(100), Some("beta"), Seq(
        SubPiece("one", Bytes(0), Atom(Bytes(10), Some("gamma"))),
        SubPiece("two", Bytes(50), Atom(Bytes(10), None))))

    val displayed = presentPiece(molecule)
    displayed.text mustBe "WHOLE: beta"
    displayed.color mustBe GoodColor
    displayed.getChildren.value() mustBe Seq(
      DisplayTreeNode("one: gamma", GoodColor, None),
      DisplayTreeNode("two", GoodColor, None)
    )
  }

  test("presentPiece - without value - with note") {
    presentPiece(Atom(Bytes(2), None, notes = Seq("note"))) mustBe
      DisplayTreeNode("WHOLE (note)", GoodColor, None)
  }

  test("presentPiece - with value - with note") {
    presentPiece(Atom(Bytes(2), Some("delta"), notes = Seq("note"))) mustBe
      DisplayTreeNode("WHOLE: delta (note)", GoodColor, None)
  }

  test("presentPiece - multiple notes") {
    presentPiece(Atom(Bytes(2), None, notes = Seq("note 1", "note 2"))) mustBe
      DisplayTreeNode("WHOLE (note 1; note 2)", GoodColor, None)
  }

  test("presentPiece - varying quality") {
    for (quality <- PieceQuality.values)
      presentPiece(Atom(Bytes(2), None, quality = quality)) mustBe
        DisplayTreeNode("WHOLE", QualityColors(quality), None)
  }
}

object PackageSuite {
  private val GoodColor = QualityColors(PieceQuality.Good)
}
