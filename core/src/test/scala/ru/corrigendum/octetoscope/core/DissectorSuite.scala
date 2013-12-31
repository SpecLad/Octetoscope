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

import org.scalatest.FunSuite
import org.scalatest.Matchers._
import ru.corrigendum.octetoscope.abstractinfra.Blob
import ru.corrigendum.octetoscope.core.mocks.MockDissector

class DissectorSuite extends FunSuite {
  test("Dissector.dissectO") {
    val blob = new ArrayBlob(Array[Byte](1, 2))
    val result = MockDissector.dissect(blob)
    val resultO = MockDissector.dissectO(blob)

    resultO._1 shouldBe result._1
    resultO._2 shouldBe Some(result._2)
  }

  test("MoleculeBuilderDissector to Dissector") {
    val mbd = new MoleculeBuilderDissector[Int] {
      def dissect(input: Blob, offset: InfoSize, builder: MoleculeBuilder) = {
        builder.addChild("alpha", Bytes(1), Atom(Bytes(1), Some("a")))
        60
      }
    }

    MoleculeBuilderDissector.toDissector(mbd).dissect(Blob.empty) shouldBe
      (
        Molecule(Bytes(2), None, Seq(
          SubPiece("alpha", Bytes(1), Atom(Bytes(1), Some("a")))
        )),
        60
      )
  }
}
