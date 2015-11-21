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

class ConstraintSuite extends FunSuite {
  test("ShouldMustConstraint") {
    val constraint = new ShouldMustConstraint[Nothing] {
      override def shouldNote: String = "should"
      override def mustNote: String = "must"
      override def check(value: Nothing): Boolean = false
    }

    // Doesn't really make sense to constrain with an informational note,
    // but let's test it anyway.
    constraint.note(NoteSeverity.Info) mustBe "should"
    constraint.note(NoteSeverity.Warning) mustBe "should"
    constraint.note(NoteSeverity.Error) mustBe "must"
    constraint.note(NoteSeverity.Failure) mustBe "must"
  }

  test("Constraint.apply") {
    val piece = Atom(InfoSize(), EmptyContents)

    val c1 = new Constraint[Any] {
      override def check(value: Any): Boolean = true
      override def note(severity: NoteSeverity.Value): String = throw new NotImplementedError()
    }
    val c2 = new Constraint[Any] {
      override def check(value: Any): Boolean = false
      override def note(severity: NoteSeverity.Value): String = severity.toString
    }

    c1.apply(piece, NoteSeverity.Error) mustBe piece
    c2.apply(c2.apply(piece, NoteSeverity.Info), NoteSeverity.Warning) mustBe
      piece.withNote(Note(NoteSeverity.Info, "Info")).withNote(Note(NoteSeverity.Warning, "Warning"))
  }
}
