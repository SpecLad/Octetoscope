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

package ru.corrigendum.octetoscope.core

import org.scalatest.FunSuite
import org.scalatest.MustMatchers._

class ConstraintSuite extends FunSuite {
  test("ShouldMustConstraint") {
    val constraint = new ShouldMustConstraint[Nothing] {
      override def shouldNote: String = "should"
      override def mustNote: String = "must"
      override def check(value: Nothing): Boolean = false
    }

    // Doesn't really make sense to constrain with the Good quality,
    // but let's test it anyway.
    constraint.note(Quality.Good) mustBe "should"
    constraint.note(Quality.Dubious) mustBe "should"
    constraint.note(Quality.Bad) mustBe "must"
    constraint.note(Quality.Broken) mustBe "must"
  }

  test("Constraint.apply") {
    val piece = Atom(InfoSize(), EmptyContents)

    val c1 = new Constraint[Any] {
      override def check(value: Any): Boolean = true
      override def note(quality: Quality.Value): String = throw new NotImplementedError()
    }
    val c2 = new Constraint[Any] {
      override def check(value: Any): Boolean = false
      override def note(quality: Quality.Value): String = quality.toString
    }

    c1.apply(piece, Quality.Bad) mustBe piece
    c2.apply(c2.apply(piece, Quality.Good), Quality.Dubious) mustBe
      piece.withNote(Note(Quality.Good, "Good")).withNote(Note(Quality.Dubious, "Dubious"))
  }
}
