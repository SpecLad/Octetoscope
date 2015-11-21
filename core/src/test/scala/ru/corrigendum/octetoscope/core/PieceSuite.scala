/*
  This file is part of Octetoscope.
  Copyright (C) 2013-2015 Octetoscope contributors (see /AUTHORS.txt)

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
import org.scalatest.MustMatchers._

class PieceSuite extends FunSuite {
  test("withNote") {
    val note1 = Note(NoteSeverity.Warning, "foo")
    val note2 = Note(NoteSeverity.Error, "bar")
    Atom(InfoSize(), EmptyContents, notes = Seq(note1)).withNote(note2).notes mustBe Seq(note1, note2)
    Molecule(InfoSize(), EmptyContents, Seq(), notes = Seq(note2)).withNote(note1).notes mustBe Seq(note2, note1)
  }

  test("ToStringContents") {
    val obj = new Object {
      override def toString: String = "foo"
    }
    val cont = new ToStringContents[Object](obj)
    cont.value must be theSameInstanceAs obj
    cont.repr mustBe "foo"
  }

  test("Contents.equals") {
    val cont1 = new Contents[Int](5) {
      override def reprO: Option[String] = Some("abc")
    }
    val cont2 = new Contents[Short](5) {
      override def reprO: Option[String] = Some("abc")
    }
    val cont3 = new Contents[Int](4) {
      override def reprO: Option[String] = Some("abc")
    }
    val cont4 = new Contents[Int](5) {
      override def reprO: Option[String] = Some("def")
    }

    cont1.equals(null) mustBe false
    cont1.equals(new Object) mustBe false
    cont1.equals(cont2) mustBe true
    cont1.equals(cont3) mustBe false
    cont1.equals(cont4) mustBe false
  }

  test("Contents.toString") {
    val cont = new Contents[Int](5) {
      override def reprO: Option[String] = Some("abc")
    }
    cont.toString mustBe "Contents(5, Some(abc))"
  }

  test("ContentsR.reprO") {
    val cont = new ContentsR[Int](0) {
      override def repr: String = "foo"
    }

    cont.reprO mustBe Some("foo")
  }
}
