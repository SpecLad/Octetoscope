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
import org.scalatest.matchers.ShouldMatchers._

class ConstraintSuite extends FunSuite {
  test("ShouldMustConstraint") {
    val constraint = new ShouldMustConstraint[Nothing] {
      override def shouldNote: String = "should"
      override def mustNote: String = "must"
      override def check(value: Nothing): Boolean = false
    }

    // Doesn't really make sense to constrain with the Good quality,
    // but let's test it anyway.
    constraint.note(PieceQuality.Good) should equal ("should")
    constraint.note(PieceQuality.Dubious) should equal ("should")
    constraint.note(PieceQuality.Bad) should equal ("must")
    constraint.note(PieceQuality.Broken) should equal ("must")
  }
}
