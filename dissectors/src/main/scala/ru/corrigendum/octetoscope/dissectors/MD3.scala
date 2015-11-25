/*
  This file is part of Octetoscope.
  Copyright (C) 2015 Octetoscope contributors (see /AUTHORS.txt)

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

package ru.corrigendum.octetoscope.dissectors

import ru.corrigendum.octetoscope.core.CommonConstraints._
import ru.corrigendum.octetoscope.core.CompoundDissectors._
import ru.corrigendum.octetoscope.core.PrimitiveDissectors._
import ru.corrigendum.octetoscope.core._

private[dissectors] object MD3 extends MoleculeBuilderUnitDissector {
  val MagicBytes = Array[Byte]('I', 'D', 'P', '3')

  private class HeaderValue {
    var numFrames: Option[Int] = None
    var numTags: Option[Int] = None
    var numSurfaces: Option[Int] = None
    var offFrames: Option[Int] = None
    var offTags: Option[Int] = None
    var offSurfaces: Option[Int] = None
    var fileSize: Option[Int] = None
  }

  // Quake III's md3Header_t
  private object Header extends SimpleMoleculeBuilderDissector[HeaderValue] {
    override def defaultWIP: HeaderValue = new HeaderValue

    override def dissectMB(context: DissectionContext, offset: InfoSize,
                           builder: MoleculeBuilder, wip: HeaderValue): Unit = {
      val add = new SequentialAdder(context, offset, builder)

      val correctMagic = add("Identification", magic(MagicBytes, "IDP3")).isDefined
      if (!correctMagic) return

      val version = add("Version", sInt32L + equalTo(15, "MD3_VERSION"))
      if (version != 15) return

      add("Name", asciiishZString(64))
      add("Flags", bitField(32, Map.empty, unnamedReason = "unused"))

      wip.numFrames = add.filtered("Number of frames", sInt32L +? noMoreThan(1024, "MD3_MAX_FRAMES"))(nonNegative)
      wip.numTags = add.filtered("Number of tags", sInt32L +? noMoreThan(16, "MD3_MAX_TAGS"))(nonNegative)
      wip.numSurfaces = add.filtered("Number of surfaces", sInt32L +? noMoreThan(32, "MD3_MAX_SURFACES"))(nonNegative)

      // though it is unused, being a count it presumably must still be >= 0
      add("Number of skins (unused)", sInt32L + nonNegative)

      wip.offFrames = add.filtered("Offset of frames", sInt32L)(nonNegative)
      wip.offTags = add.filtered("Offset of tags", sInt32L)(nonNegative)
      wip.offSurfaces = add.filtered("Offset of surfaces", sInt32L)(nonNegative)

      val softSize = (context.softLimit - offset).bytes
      val fileSizeConstraint = if (softSize < Int.MaxValue)
        noMoreThan(softSize.toInt, "actual file size") else any

      wip.fileSize = add.filtered("File size", sInt32L)(nonNegative, fileSizeConstraint)
    }
  }

  override def dissectMBU(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder): Unit = {
    val add = new RandomAdder(context, offset, builder)
    val header = add("Header", Bytes(0), Header)

    builder.setRepr("Quake III model")

    for (fileSize <- header.fileSize)
      builder.fixSize(Bytes(fileSize))
  }
}
