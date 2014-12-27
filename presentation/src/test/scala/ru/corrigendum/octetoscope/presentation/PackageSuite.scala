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
import ru.corrigendum.octetoscope.abstractinfra.Blob
import ru.corrigendum.octetoscope.core._
import ru.corrigendum.octetoscope.presentation.tools.DisplayTreeNodeData

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
    DisplayTreeNodeData.from(presentPieceIgnoringEvents(Atom(Bytes(5), new EagerContentsR((), "alpha")))) mustBe
      DisplayTreeNodeData("WHOLE: alpha", Nil)
  }

  test("presentPiece - atom - without value") {
    DisplayTreeNodeData.from(presentPieceIgnoringEvents(Atom(Bytes(2), EmptyContents))) mustBe
      DisplayTreeNodeData("WHOLE", Nil)
  }

  test("presentPiece - molecule") {
    val molecule =
      Molecule(Bytes(100), new EagerContentsR((), "beta"), Seq(
        SubPiece("one", Bytes(0), Atom(Bytes(10), new EagerContentsR((), "gamma"))),
        SubPiece("two", Bytes(50), Atom(Bytes(10), EmptyContents))))

    DisplayTreeNodeData.from(presentPieceIgnoringEvents(molecule)) mustBe
      DisplayTreeNodeData("WHOLE: beta", Nil, Some(Seq(
        DisplayTreeNodeData("one: gamma", Nil),
        DisplayTreeNodeData("two", Nil)
      ))
    )
  }

  test("presentPiece - with note") {
    for (quality <- Quality.values) {
      val piece = Atom(Bytes(2), EmptyContents, notes = Seq(Note(quality, "note")))
      DisplayTreeNodeData.from(presentPieceIgnoringEvents(piece)) mustBe
        DisplayTreeNodeData("WHOLE", Seq((QualityColors(quality), "note")))
    }
  }

  test("presentPiece - multiple notes") {
    val actual = DisplayTreeNodeData.from(presentPieceIgnoringEvents(Atom(Bytes(2), EmptyContents, notes =
      Seq(Note(Quality.Good, "note 1"), Note(Quality.Bad, "note 2")))))
    val expected = DisplayTreeNodeData("WHOLE",
      Seq((QualityColors(Quality.Good), "note 1"), (QualityColors(Quality.Bad), "note 2")))
    actual mustBe expected
  }

  test("presentPiece - double click handler") {
    val piece = Molecule(Bytes(100), EmptyContents, Seq(
      SubPiece("alpha", Bytes(50), Molecule(Bytes(25), EmptyContents, Seq(
        SubPiece("beta", Bytes(5), Atom(Bytes(10), EmptyContents))
      )))
    ))

    var receivedOffset: InfoSize = null
    var receivedSize: InfoSize = null

    def handleDoubleClick(offset: InfoSize, size: InfoSize) {
      receivedOffset = offset
      receivedSize = size
    }

    val displayTreeNode = presentPiece(piece, handleDoubleClick)
    displayTreeNode.getChildren.get()(0).getChildren.get()(0).eventListener.doubleClicked()

    receivedOffset mustBe Bytes(55)
    receivedSize mustBe Bytes(10)
  }

  test("presentBlobAsHexadecimal - empty") {
    presentBlobAsHexadecimal(Blob.empty, 10) mustBe ""
  }

  test("presentBlobAsHexadecimal - multiple") {
    val blob = new ArrayBlob(Array[Byte](0x12, 0x23, 0x34, 0x4a, 0x5b, 0x6c))
    presentBlobAsHexadecimal(blob, 2) mustBe "12 23\n34 4a\n5b 6c"
  }

  test("presentBlobAsHexadecimal - non-multiple") {
    val blob = new ArrayBlob(Array[Byte](0x12, 0x23, 0x34, 0x4a, 0x5b, 0x6c, 0))
    presentBlobAsHexadecimal(blob, 3) mustBe "12 23 34\n4a 5b 6c\n00"
  }

  test("generateBlobOffsets") {
    generateBlobOffsets(0, 4) mustBe ""
    generateBlobOffsets(1, 4) mustBe "00"
    generateBlobOffsets(4, 4) mustBe "00"
    generateBlobOffsets(7, 4) mustBe "00\n04"
    generateBlobOffsets(9, 4) mustBe "00\n04\n08"
    generateBlobOffsets(0x101, 0x40) mustBe "0000\n0040\n0080\n00c0\n0100"
    generateBlobOffsets(0x15007, 0x3001) mustBe
      "00000000\n00003001\n00006002\n00009003\n0000c004\n0000f005\n00012006"
    generateBlobOffsets(0x200000000L, 0x7fffffff) mustBe
      "0000000000000000\n000000007fffffff\n00000000fffffffe\n000000017ffffffd\n00000001fffffffc"
  }
}

object PackageSuite {
  def presentPieceIgnoringEvents(piece: PlainPiece) = presentPiece(piece, (offset: InfoSize, size: InfoSize) => ())
}
