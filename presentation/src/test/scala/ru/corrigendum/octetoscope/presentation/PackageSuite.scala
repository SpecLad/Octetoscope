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
import ru.corrigendum.octetoscope.abstractui.DisplayTreeNode
import ru.corrigendum.octetoscope.core._

class PackageSuite extends FunSuite {
  test("presentVersionInfo") {
    val hash = "1234" * 10
    presentVersionInfo(VersionInfo("1.2", 0, hash, dirty = false)) mustBe "1.2-g1234123"
    presentVersionInfo(VersionInfo("1.2", 34, hash, dirty = false)) mustBe "1.2+34-g1234123"
    presentVersionInfo(VersionInfo("1.2", 0, hash, dirty = true)) mustBe "1.2-g1234123-dirty"
    presentVersionInfo(VersionInfo("1.2", 34, hash, dirty = true)) mustBe "1.2+34-g1234123-dirty"
  }

  test("presentPiece - atom - with value") {
    presentPiece(Atom(Bytes(5), new EagerContentsR((), "alpha"))) mustBe DisplayTreeNode("WHOLE: alpha", Nil, None)
  }

  test("presentPiece - atom - without value") {
    presentPiece(Atom(Bytes(2), EmptyContents)) mustBe DisplayTreeNode("WHOLE", Nil, None)
  }

  test("presentPiece - molecule") {
    val molecule =
      Molecule(Bytes(100), new EagerContentsR((), "beta"), Seq(
        SubPiece("one", Bytes(0), Atom(Bytes(10), new EagerContentsR((), "gamma"))),
        SubPiece("two", Bytes(50), Atom(Bytes(10), EmptyContents))))

    val displayed = presentPiece(molecule)
    displayed.text mustBe "WHOLE: beta"
    displayed.notes mustBe Nil
    displayed.getChildren.value() mustBe Seq(
      DisplayTreeNode("one: gamma", Nil, None),
      DisplayTreeNode("two", Nil, None)
    )
  }

  test("presentPiece - with note") {
    for (quality <- Quality.values)
      presentPiece(Atom(Bytes(2), EmptyContents, notes = Seq(Note(quality, "note")))) mustBe
        DisplayTreeNode("WHOLE", Seq((QualityColors(quality), "note")), None)
  }

  test("presentPiece - multiple notes") {
    val actual = presentPiece(Atom(Bytes(2), EmptyContents, notes =
      Seq(Note(Quality.Good, "note 1"), Note(Quality.Bad, "note 2"))))
    val expected = DisplayTreeNode("WHOLE",
      Seq((QualityColors(Quality.Good), "note 1"), (QualityColors(Quality.Bad), "note 2")), None)
    actual mustBe expected
  }
}
