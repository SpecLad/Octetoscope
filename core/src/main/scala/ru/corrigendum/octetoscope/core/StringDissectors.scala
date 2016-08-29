/*
  This file is part of Octetoscope.
  Copyright (C) 2014-2016 Octetoscope contributors (see /AUTHORS.txt)

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

import java.nio.charset.{Charset, CoderResult}
import java.nio.{ByteBuffer, CharBuffer}
import java.util.StringTokenizer

import scala.util.control.Breaks._

private object StringDissectors {
  /* The double quote is used to delimit printable characters in string reprs,
     so to avoid ambiguity we treat it as a special character. The rest are
     simply non-printable. */
  private val AsciiSpecialChars = Map('\u007f' -> "DEL", '"' -> "QUOTE") ++
    Seq("NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
      "BS", "HT", "LF", "VT", "FF", "CR", "SO", "SI",
      "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
      "CAN", "EM", "SUB", "ESC", "FS", "GS", "RS", "US").zipWithIndex.map(pair => (pair._2.toChar, pair._1))

  private def readString(inBuffer: ByteBuffer, encoding: Charset): ContentsR[Option[String]] = {
    val outBuffer = CharBuffer.allocate(256)
    val decoder = encoding.newDecoder()

    type Chunk = Either[IndexedSeq[Byte], String]

    val chunksBuilder = List.newBuilder[Chunk]

    breakable {
      while (true) {
        val cr = decoder.decode(inBuffer, outBuffer, true)

        outBuffer.flip()
        if (outBuffer.hasRemaining)
          chunksBuilder += Right(outBuffer.toString)
        outBuffer.clear()

        cr match {
          case CoderResult.UNDERFLOW =>
            break()
          case CoderResult.OVERFLOW =>
          // don't need to do anything
          case _ =>
            val undecodedBytes = new Array[Byte](cr.length())
            inBuffer.get(undecodedBytes)
            chunksBuilder += Left(undecodedBytes)
        }
      }
    }

    val chunks = chunksBuilder.result()

    def groupChunks(chunks: List[Chunk]): List[Chunk] = chunks match {
      case Nil => Nil
      case Left(_) :: _ =>
        val (lefts, rest) = chunks.span(_.isLeft)
        Left(lefts.flatMap(_.left.get)(collection.breakOut)) :: groupChunks(rest)
      case Right(_) :: _ =>
        val (rights, rest) = chunks.span(_.isRight)
        Right(rights.map(_.right.get).mkString) :: groupChunks(rest)
    }

    val groupedChunks = groupChunks(chunks)

    val value = groupedChunks match {
      case Seq() => Some("")
      case Seq(Right(str)) => Some(str)
      case _ => None
    }

    def stringifyDecodedChunk(chunk: String): String = {
      import scala.collection.JavaConverters._
      // TODO: at some point, we'll need to also have special representations for Unicode control characters.
      // Currently, though, no dissectors can encounter those.
      val tokenizer = new StringTokenizer(chunk, AsciiSpecialChars.keys.mkString, true)
      tokenizer.asScala.map { o =>
        val s = o.toString
        AsciiSpecialChars.getOrElse(s.head, "\"" + s + "\"")
      }.mkString(" ")
    }

    def stringifyChunk(chunk: Chunk): String =
      chunk.fold("0x" + _.map("%02x".format(_)).mkString, stringifyDecodedChunk)

    new EagerContentsR(value,
        if (groupedChunks.isEmpty) "\"\""
        else groupedChunks.map(stringifyChunk).mkString(" "))
  }

  private val noteMissingTerminator = Note(NoteSeverity.Error, "missing NUL terminator")
  private val noteInvalidEncoding = Note(NoteSeverity.Failure, "invalid encoding")

  class SizedString(encoding: Charset, length: Int, allowDecodingErrors: Boolean)
      extends DissectorCR[Option[String]] {
    final override def dissect(context: DissectionContext, offset: InfoSize): AtomCR[Option[String]] = {
      val Bytes(bo) = offset

      val inBuffer = ByteBuffer.wrap(context.input.getRangeAsArray(bo, bo + length))
      val contents = readString(inBuffer, encoding)

      Atom(
        Bytes(length),
        contents,
        if (contents.value.isEmpty && !allowDecodingErrors) Seq(noteInvalidEncoding) else Nil)
    }
  }

  class SizedZString(encoding: Charset, length: Int, allowDecodingErrors: Boolean)
      extends DissectorCR[Option[String]] {
    final override def dissect(context: DissectionContext, offset: InfoSize): AtomCR[Option[String]] = {
      val Bytes(bo) = offset

      // read all `length` bytes in order to correctly trigger an exception if the blob is big enough
      // to contain the string plus terminator, but not the entire piece
      val range = context.input.getRangeAsArray(bo, bo + length)
      val nulIndex = range.indexOf(0)
      val inBuffer = ByteBuffer.wrap(range, 0, if (nulIndex >= 0) nulIndex else range.length)
      val contents = readString(inBuffer, encoding)

      var notes: Seq[Note] = Nil

      if (nulIndex < 0)
        notes = noteMissingTerminator +: notes

      if (contents.value.isEmpty && !allowDecodingErrors)
        notes = noteInvalidEncoding +: notes

      Atom(Bytes(length), contents, notes)
    }
  }

  class ZString(encoding: Charset) extends DissectorCR[Option[String]] {
    override def dissect(context: DissectionContext, offset: InfoSize): Piece[ContentsR[Option[String]]] = {
      val Bytes(bo) = offset

      var nulIndex = 0L
      while (bo + nulIndex < context.softLimit.bytes && context.input(bo + nulIndex) != 0) nulIndex += 1

      val inBuffer = ByteBuffer.wrap(context.input.getRangeAsArray(bo, bo + nulIndex))
      val contents = readString(inBuffer, encoding)

      val nulMissing = bo + nulIndex == context.softLimit.bytes

      var notes: Seq[Note] = Nil

      if (nulMissing)
        notes = noteMissingTerminator +: notes

      if (contents.value.isEmpty)
        notes = noteInvalidEncoding +: notes

      Atom(Bytes(if (nulMissing) nulIndex else nulIndex + 1), contents, notes)
    }
  }
}
