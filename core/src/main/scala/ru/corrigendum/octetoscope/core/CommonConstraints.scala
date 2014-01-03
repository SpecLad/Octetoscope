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

private object CommonConstraints {
  def nonNegative[T](implicit arithm: Numeric[T]) = new ShouldMustConstraint[T] {
    override def check(value: T): Boolean = arithm.signum(value) >= 0
    def shouldNote: String = "should be non-negative"
    def mustNote: String = "must be non-negative"
  }

  def positive[T](implicit arithm: Numeric[T]) = new ShouldMustConstraint[T] {
    override def check(value: T): Boolean = arithm.signum(value) > 0
    def shouldNote: String = "should be positive"
    def mustNote: String = "must be positive"
  }

  def equalTo[T](expected: T, meaning: String) = new ShouldMustConstraint[T] {
    override def shouldNote: String = "should equal %s (%s)".format(expected, meaning)
    override def mustNote: String = "must equal %s (%s)".format(expected, meaning)
    override def check(value: T): Boolean = value == expected
  }

  def noMoreThan[T](limit: T, meaning: String)(implicit ord: Ordering[T]) = new ShouldMustConstraint[T] {
    override def shouldNote: String = "should be no more than %s (%s)".format(limit, meaning)
    override def mustNote: String = "must be no more than %s (%s)".format(limit, meaning)
    override def check(value: T): Boolean = ord.lteq(value, limit)
  }
}
