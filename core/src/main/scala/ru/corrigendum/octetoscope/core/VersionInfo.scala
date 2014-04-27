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

import java.util.Properties
import resource.managed

case class VersionInfo(releaseVersion: String, extraCommits: Int, commitHash: String, dirty: Boolean)

object VersionInfo {
  val ours = {
    val props = new Properties()

    val resStream = getClass.getResourceAsStream("version.properties")

    if (resStream ne null)
      for (_ <- managed(resStream))
        props.load(resStream)

    VersionInfo(
      releaseVersion = props.getProperty("releaseVersion", "unknown"),
      extraCommits = Option(props.getProperty("extraCommits")).fold(0)(_.toInt),
      commitHash = props.getProperty("commitHash", "0" * 40),
      dirty = Option(props.getProperty("dirty")).exists(_.toBoolean)
    )
  }
}
