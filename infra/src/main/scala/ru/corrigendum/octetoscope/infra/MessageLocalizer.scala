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

package ru.corrigendum.octetoscope.infra

import java.lang.reflect.{Method, InvocationHandler, Proxy}
import com.ibm.icu.util.ULocale
import scala.Array
import ru.corrigendum.octetoscope.abstractinfra.Translation
import com.ibm.icu.text.MessageFormat

object MessageLocalizer {
  def localize[T](iface: Class[T], translationMap: Map[String, Class[_ <: T]]): T = {
    val locale = ULocale.acceptLanguage(Array(ULocale.getDefault),
      translationMap.keys.map(ULocale.forLanguageTag).toArray, null)

    val transIface = translationMap.getOrElse(locale.toLanguageTag, iface)

    val translations = Map((for {
      meth <- transIface.getMethods
      ann = meth.getAnnotation(classOf[Translation])
      if ann ne null
    } yield (meth.getName, ann.format())): _*)

    iface.cast(Proxy.newProxyInstance(transIface.getClassLoader, Array(transIface), new InvocationHandler {
      override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef =
        MessageFormat.format(translations(method.getName), args: _*)
    }))
  }
}
