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

import java.text.SimpleDateFormat
import java.util.{Locale, TimeZone, Date}

import ru.corrigendum.octetoscope.core._
import ru.corrigendum.octetoscope.core.CommonConstraints._
import ru.corrigendum.octetoscope.core.CompoundDissectors._
import ru.corrigendum.octetoscope.core.PrimitiveDissectors._
import ru.corrigendum.octetoscope.core.SpecialDissectors._

/*
  Gzip compression format.

  Dissection is based on RFC 1952 (https://www.ietf.org/rfc/rfc1952.pdf),
  as supplemented by the description at http://www.gzip.org/format.txt (last modified on 2002-07-02).
*/


object Gzip extends MoleculeBuilderUnitDissector {
  val MagicBytes = Array(31.toByte, 139.toByte)

  private object MTime extends DissectorCR[Unit] {
    private val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss", Locale.ROOT)
    dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"))

    override def dissect(context: DissectionContext, offset: InfoSize): Piece[ContentsR[Unit]] = {
      // The spec doesn't say whether the timestamp is signed or unsigned.
      // It does say that it's "in Unix format", though, and Unix time is usually signed.
      val intPiece = sInt32L.dissect(context, offset)

      Atom(Bytes(4), new ContentsR(()) {
        override def repr: String = {
          val timeRepr = if (intPiece.contents.value == 0) "no time stamp available"
            else dateFormat.format(new Date(intPiece.contents.value * 1000L))
          intPiece.contents.repr + " - " + timeRepr
        }
      })
    }
  }

  private object ExtraSubfield extends MoleculeBuilderUnitDissector {
    override def dissectMBU(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder): Unit = {
      val add = new SequentialAdder(context, offset, builder)

      val knownSubfields = Map[Option[String], String](
        Some("AC") -> "Acorn RISC OS/BBC MOS file type information",
        Some("Ap") -> "Apollo file type information",
        Some("cp") -> "file compressed by cpio",
        Some("GS") -> "gzsig",
        Some("KN") -> "KeyNote assertion (RFC 2704)",
        Some("Mc") -> "Macintosh info (Type and Creator values)",
        Some("RO") -> "Acorn Risc OS file type information"
      )

      val siC = add.getContents("Subfield ID", enum(asciiishString(2), knownSubfields))
      builder.setReprLazy(siC.repr)
      val len = add("Length", uInt16L)
      add("Data", opaque(Bytes(len))) // TODO: dissect known subfields
    }
  }

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

      val compressionMethod = add("Compression method", enum(uInt8, Map((8: Short) -> "deflate")))

      val (flags, reservedFlags) = add("Flags", bitField(8,
        Map(3L -> "FCOMMENT", 4L -> "FNAME", 5L -> "FEXTRA", 6L -> "FHCRC", 7L -> "FTEXT"),
        unnamedReason = "reserved", unnamedConstraint = `false`))

      if (reservedFlags.nonEmpty) {
        builder.addNote(NoteSeverity.Failure, "a reserved header flag is set")
        return
      }

      add("Modification time", MTime)

      // The spec is unclear on whether XFL is a bit field or an enumeration.
      // On one hand, "slowest algorithm" and "fastest algorithm" seem mutually exclusive.
      // On the other hand, their values are powers of two, and gzip will use a value of
      // 0 if the algorithm used is neither slowest nor fastest. Plus the field is called "flags".
      // Since there is more evidence in favor of the bit field option, we'll assume that.

      val extraFlagNames = if (compressionMethod.contains("deflate"))
        Map(5L -> "Fastest algorithm", 6L -> "Maximum compression") else Map[Long, String]()

      add("Extra flags", bitField(8, extraFlagNames))

      add("Operating system", enum(uInt8, Map(
        (0: Short) -> "FAT file system", 1 -> "Amiga", 2 -> "VMS", 3 -> "Unix",
        4 -> "VM/CMS", 5 -> "Atari", 6 -> "HPFS file system", 7 -> "Macintosh",
        8 -> "Z-System", 9 -> "CP/M", 10 -> "TOPS-20", 11 -> "NTFS file system",
        12 -> "SMS/QDOS", 13 -> "Acorn RISC OS", 14 -> "VFAT file system", 15 -> "MVS",
        16 -> "BeOS", 17 -> "Tandem/NSK", 18 -> "THEOS", 255 -> "unknown"
      )))

      if (flags("FEXTRA")) {
        val xlen = add("Extra length", uInt16L)
        add("Extra field", fixedSize(sequence("Subfield", ExtraSubfield), Bytes(xlen)))
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
