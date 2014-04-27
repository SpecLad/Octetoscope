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

object CommonConstraints {
  private object AnyConstraint extends Constraint[Any] {
    override def check(value: Any): Boolean = true
    override def note(quality: Quality.Value): String =
      throw new IllegalStateException("note requested for the any constraint")
  }

  /*
    Constraint that always passes.

    This is handy when you want to apply a real constraint, but it
    depends on data that is not always available. Instead of complicating
    code at the point of use:
      if (haveData) dissector + realConstraint(data) else dissector
    you can create the constraint beforehand:
      constraint = if (haveData) realConstraint(data) else any
    and then at the point of use you'll have simply:
     dissector + constraint
   */
  def any: Constraint[Any] = AnyConstraint

  def nonNegative[T](implicit arithm: Numeric[T]): ShouldMustConstraint[T] = new ShouldMustConstraint[T] {
    override def check(value: T): Boolean = arithm.signum(value) >= 0
    override def shouldNote: String = "should be non-negative"
    override def mustNote: String = "must be non-negative"
  }

  def positive[T](implicit arithm: Numeric[T]): ShouldMustConstraint[T] = new ShouldMustConstraint[T] {
    override def check(value: T): Boolean = arithm.signum(value) > 0
    override def shouldNote: String = "should be positive"
    override def mustNote: String = "must be positive"
  }

  def equalTo[T](expected: T, meaning: String): ShouldMustConstraint[T] = new ShouldMustConstraint[T] {
    override def shouldNote: String = "should equal %s (%s)".format(expected, meaning)
    override def mustNote: String = "must equal %s (%s)".format(expected, meaning)
    override def check(value: T): Boolean = value == expected
  }

  def noMoreThan[T](limit: T, meaning: String)(implicit ord: Ordering[T]): ShouldMustConstraint[T] =
    new ShouldMustConstraint[T] {
      override def shouldNote: String = "should be no more than %s (%s)".format(limit, meaning)
      override def mustNote: String = "must be no more than %s (%s)".format(limit, meaning)
      override def check(value: T): Boolean = ord.lteq(value, limit)
    }

  def lessThan[T](limit: T, meaning: String)(implicit ord: Ordering[T]): ShouldMustConstraint[T] =
    new ShouldMustConstraint[T] {
      override def shouldNote: String = "should be less than %s (%s)".format(limit, meaning)
      override def mustNote: String = "must be less than %s (%s)".format(limit, meaning)
      override def check(value: T): Boolean = ord.lt(value, limit)
    }
}
