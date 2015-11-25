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

/*
  Quake III models (*.md3).

  Dissection is based on the Quake III Arena source code,
  available at <https://github.com/id-Software/Quake-III-Arena>.
*/

private[dissectors] object MD3 extends MoleculeBuilderUnitDissector {
  val MagicBytes = Array[Byte]('I', 'D', 'P', '3')

  private class HeaderValue {
    var nameC: Option[ContentsR[Any]] = None
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

      wip.nameC = Some(add.getContents("Name", asciiishZString(64)))
      add("Flags", bitField(32, Map.empty, unnamedReason = "unused"))

      wip.numFrames = add.filtered("Number of frames",
        sInt32L +? noMoreThan(1024, "MD3_MAX_FRAMES") + positive)(nonNegative)
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

  // Quake III's md3Frame_t
  private object Frame extends MoleculeBuilderUnitDissector {
    override def dissectMBU(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder): Unit = {
      val add = new SequentialAdder(context, offset, builder)

      add("Lower bound", Common.vector3(float32L))
      add("Upper bound", Common.vector3(float32L))
      add("Local origin", Common.vector3(float32L))
      add("Radius", float32L)
      val nameC = add.getContents("Name", asciiishZString(16))

      builder.setReprLazy(nameC.repr)
    }
  }

  // Quake III's md3Tag_t
  private object Tag extends MoleculeBuilderUnitDissector {
    override def dissectMBU(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder): Unit = {
      val add = new SequentialAdder(context, offset, builder)

      val nameC = add.getContents("Name", asciiishZString(64))
      builder.setReprLazy(nameC.repr)

      add("Origin", Common.vector3(float32L))
      add("X axis", Common.vector3(float32L))
      add("Y axis", Common.vector3(float32L))
      add("Z axis", Common.vector3(float32L))
    }
  }

  override def dissectMBU(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder): Unit = {
    val add = new RandomAdder(context, offset, builder)
    val header = add("Header", Bytes(0), Header)

    builder.setReprLazy(header.nameC.fold("Quake III model")("Quake III model " + _.repr))

    for (fileSize <- header.fileSize)
      builder.fixSize(Bytes(fileSize))

    for (numFrames <- header.numFrames; offFrames <- header.offFrames)
      add("Frames", Bytes(offFrames), array(numFrames, "Frame", Frame))

    for (numTags <- header.numTags; offTags <- header.offTags)
      add("Tags", Bytes(offTags), array(numTags, "Tag", Tag))
  }
}
