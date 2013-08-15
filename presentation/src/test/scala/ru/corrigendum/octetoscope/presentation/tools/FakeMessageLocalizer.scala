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

package ru.corrigendum.octetoscope.presentation.tools

import java.lang.reflect.{Method, InvocationHandler, Proxy}
import java.util

object FakeMessageLocalizer {
  def localize[T](iface: Class[T]): T = iface.cast(Proxy.newProxyInstance(iface.getClassLoader, Array(iface),
    new InvocationHandler {
      def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef =
        method.getName + " (" + (if (args eq null) "" else args.map(_.toString).mkString(", ")) +
          ") @" + (method.getName.hashCode + util.Arrays.hashCode(args)).toString
    }
  ))
}
