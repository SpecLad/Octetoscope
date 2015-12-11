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

import ru.corrigendum.octetoscope.core._
import ru.corrigendum.octetoscope.core.CommonConstraints._
import ru.corrigendum.octetoscope.core.CompoundDissectors._
import ru.corrigendum.octetoscope.core.PrimitiveDissectors._

/*
  Gzip compression format.

  Dissection is based on RFC 1952 (https://www.ietf.org/rfc/rfc1952.pdf),
  as supplemented by the description at http://www.gzip.org/format.txt (last modified on 2002-07-02).
*/


object Gzip extends MoleculeBuilderUnitDissector {
  val MagicBytes = Array(31.toByte, 139.toByte)

  // the return value is whether the member was fully dissected
  private object Member extends MoleculeBuilderDissector[Boolean, Variable[Boolean]] {
    override def defaultWIP: Variable[Boolean] = Variable(false)

    override def postProcess(wip: Variable[Boolean]): Boolean = wip.value

    override def dissectMB(context: DissectionContext, offset: InfoSize,
                           builder: MoleculeBuilder, wip: Variable[Boolean]): Unit = {
      val add = new SequentialAdder(context, offset, builder)

      val correctMagic = add("Identification", magic(MagicBytes, "31 139")).isDefined
      if (!correctMagic) {
        builder.addNote(NoteSeverity.Failure, "incorrect identification")
        return
      }

      val compressionMethod = add("Compression method", enum(uInt8, Map(8.toShort -> "deflate")))

      val (flags, reservedFlags) = add("Flags", bitField(8,
        Map(3L -> "FCOMMENT", 4L -> "FNAME", 5L -> "FEXTRA", 6L -> "FHCRC", 7L -> "FTEXT"),
        unnamedReason = "reserved", unnamedConstraint = `false`))

      if (reservedFlags.nonEmpty) {
        builder.addNote(NoteSeverity.Failure, "a reserved header flag is set")
        return
      }

      // TODO: complete this
    }
  }

  override def dissectMBU(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder): Unit = {
    val add = new SequentialAdder(context, offset, builder)

    builder.setRepr("gzip compressed file")

    var memberIsComplete = true
    while (memberIsComplete && !add.limitReached) memberIsComplete = add("Member", Member)
  }
}
