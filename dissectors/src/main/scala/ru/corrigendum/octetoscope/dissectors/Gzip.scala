/*
  This file is part of Octetoscope.
  Copyright (C) 2015-2016 Octetoscope contributors (see /AUTHORS.txt)

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

import java.nio.charset.StandardCharsets
import java.text.SimpleDateFormat
import java.util.{Date, Locale, TimeZone}

import ru.corrigendum.octetoscope.core.CommonConstraints._
import ru.corrigendum.octetoscope.core.CompoundDissectors._
import ru.corrigendum.octetoscope.core.PrimitiveDissectors._
import ru.corrigendum.octetoscope.core.SpecialDissectors._
import ru.corrigendum.octetoscope.core._

/*
  Gzip compression format.

  Dissection is based on RFC 1952 (https://www.ietf.org/rfc/rfc1952.pdf),
  as supplemented by the description at http://www.gzip.org/format.txt (last modified on 2002-07-02).

  Random Access extension dissection is based on the dictzip 1.12.1 manpage (http://www.dict.org/).
  BZGF extension dissection is based on the SAM/BAM specification, last modified on 18 Nov 2015
  (https://samtools.github.io/hts-specs/SAMv1.pdf).
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
    private object Acorn extends MoleculeBuilderUnitDissector {
      override def dissectMBU(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder): Unit = {
        context.untested()

        val add = new SequentialAdder(context, offset, builder)

        // TODO: implement the alternate interpretation (when the top of the load address is 0xFFF)
        add("Load address", uInt32L)
        add("Execution address", uInt32L)
        add("Object attributes", bitField(32, Map(
          26L -> "Public write", 27L -> "Public read", 28L -> "Locked", 30L -> "Owner write", 31L -> "Owner read")))
        add("Object length", uInt32L)
        add("Reserved", opaque(Bytes(12)))
      }
    }

    private object Cpio extends MoleculeBuilderUnitDissector {
      override def dissectMBU(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder): Unit = {
        context.untested()

        val add = new SequentialAdder(context, offset, builder)
        // TODO: use this to constrain the FNAME field
        add("Length of FNAME field", uInt16L)
      }
    }

    private object RandomAccess extends MoleculeBuilderUnitDissector {
      override def dissectMBU(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder): Unit = {
        val add = new SequentialAdder(context, offset, builder)

        val ver = add("Version", uInt16L)
        if (ver != 1) {
          builder.addNote(NoteSeverity.Failure, "unsupported version")
          return
        }

        add("Chunk length", uInt16L)
        val chunkCount = add("Chunk count", uInt16L)
        add("Chunk lengths after compression", array(chunkCount, "Length", uInt16L))
      }
    }

    private object BZGF extends MoleculeBuilderUnitDissector {
      override def dissectMBU(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder): Unit = {
        val add = new SequentialAdder(context, offset, builder)
        add("Member size minus 1", uInt16L)
      }
    }

    private case class SubfieldType(name: String, dissector: Option[DissectorWithDefaultValueC[Unit]] = None) {
      override def toString = name
    }

    private val knownSubfields = Map[Option[String], SubfieldType](
      // registered subfields
      Some("AC") -> SubfieldType("Acorn RISC OS/BBC MOS file type information", Some(Acorn)),
      Some("Ap") -> SubfieldType("Apollo file type information"),
      Some("cp") -> SubfieldType("file compressed by cpio", Some(Cpio)),
      Some("GS") -> SubfieldType("gzsig"),
      Some("KN") -> SubfieldType("KeyNote assertion (RFC 2704)"),
      Some("Mc") -> SubfieldType("Macintosh info (Type and Creator values)"),
      Some("RO") -> SubfieldType("Acorn Risc OS file type information"),

      // unregistered subfields
      Some("BC") -> SubfieldType("BZGF (BAM)", Some(BZGF)),
      Some("RA") -> SubfieldType("Random Access (dictzip)", Some(RandomAccess))
    )

    private val subfieldIdDissector = enum(asciiishString(2), knownSubfields)

    override def dissectMBU(context: DissectionContext, offset: InfoSize, builder: MoleculeBuilder): Unit = {
      val add = new SequentialAdder(context, offset, builder)

      val siC = add.getContents("Subfield ID", subfieldIdDissector)
      builder.setReprLazy(siC.repr)
      val len = add("Length", uInt16L)
      add("Data", fixedSize(
        siC.value.fold(opaque)(_.dissector.getOrElse(opaque)),
        Bytes(len))
      )

      // Possible improvement: the BC and RA chunks allow us to determine the compressed size
      // of the member. We can use it to set the size of the corresponding piece so that subsequent
      // members can be dissected even if the current member dissection fails (e.g. if it's
      // corrupted or uses an unknown compression method).
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

      if (flags("FNAME")) {
        val fname = add.getContents("Original file name", zString(StandardCharsets.ISO_8859_1)).repr
        builder.setRepr(fname)
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
