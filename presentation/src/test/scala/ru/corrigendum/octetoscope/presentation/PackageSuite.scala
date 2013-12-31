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
import org.scalatest.Matchers._
import ru.corrigendum.octetoscope.core._
import ru.corrigendum.octetoscope.abstractui.DisplayTreeNode

class PackageSuite extends FunSuite {
  import PackageSuite._

  test("presentVersionInfo") {
    val hash = "1234" * 10
    presentVersionInfo(VersionInfo("1.2", 0, hash, dirty = false)) shouldBe "1.2-g1234123"
    presentVersionInfo(VersionInfo("1.2", 34, hash, dirty = false)) shouldBe "1.2+34-g1234123"
    presentVersionInfo(VersionInfo("1.2", 0, hash, dirty = true)) shouldBe "1.2-g1234123-dirty"
    presentVersionInfo(VersionInfo("1.2", 34, hash, dirty = true)) shouldBe "1.2+34-g1234123-dirty"
  }

  test("presentPiece - atom - with value") {
    presentPiece(Atom(Bytes(5), Some("alpha"))) shouldBe DisplayTreeNode("WHOLE: alpha", GoodColor, Nil)
  }

  test("presentPiece - atom - without value") {
    presentPiece(Atom(Bytes(2), None)) shouldBe DisplayTreeNode("WHOLE", GoodColor, Nil)
  }

  test("presentPiece - molecule") {
    val molecule =
      Molecule(Bytes(100), Some("beta"), Seq(
        SubPiece("one", Bytes(0), Atom(Bytes(10), Some("gamma"))),
        SubPiece("two", Bytes(50), Atom(Bytes(10), None))))

    val displayed =
      DisplayTreeNode("WHOLE: beta", GoodColor, Seq(
        DisplayTreeNode("one: gamma", GoodColor, Nil),
        DisplayTreeNode("two", GoodColor, Nil)
      ))

    presentPiece(molecule) shouldBe displayed
  }

  test("presentPiece - without value - with note") {
    presentPiece(Atom(Bytes(2), None, notes = Seq("note"))) shouldBe
      DisplayTreeNode("WHOLE (note)", GoodColor, Nil)
  }

  test("presentPiece - with value - with note") {
    presentPiece(Atom(Bytes(2), Some("delta"), notes = Seq("note"))) shouldBe
      DisplayTreeNode("WHOLE: delta (note)", GoodColor, Nil)
  }

  test("presentPiece - multiple notes") {
    presentPiece(Atom(Bytes(2), None, notes = Seq("note 1", "note 2"))) shouldBe
      DisplayTreeNode("WHOLE (note 1; note 2)", GoodColor, Nil)
  }

  test("presentPiece - varying quality") {
    for (quality <- PieceQuality.values)
      presentPiece(Atom(Bytes(2), None, quality = quality)) shouldBe
        DisplayTreeNode("WHOLE", QualityColors(quality), Nil)
  }
}

object PackageSuite {
  private val GoodColor = QualityColors(PieceQuality.Good)
}
