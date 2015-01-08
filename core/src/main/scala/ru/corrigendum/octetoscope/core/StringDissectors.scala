/*
  This file is part of Octetoscope.
  Copyright (C) 2014-2015 Octetoscope contributors (see /AUTHORS.txt)

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

import java.nio.charset.{CoderResult, StandardCharsets}
import java.nio.{ByteBuffer, CharBuffer}
import java.util.StringTokenizer

import ru.corrigendum.octetoscope.abstractinfra.Blob

import scala.util.control.Breaks._

private object StringDissectors {
  /* The double quote is used to delimit printable characters in string reprs,
     so to avoid ambiguity we treat it as a special character. The rest are
     simply non-printable. */
  val AsciiSpecialChars = Map('\u007f' -> "DEL", '"' -> "QUOTE") ++
    Seq("NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
      "BS", "HT", "LF", "VT", "FF", "CR", "SO", "SI",
      "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
      "CAN", "EM", "SUB", "ESC", "FS", "GS", "RS", "US").zipWithIndex.map(pair => (pair._2.toChar, pair._1))

  abstract class AsciiStringGeneric(length: Int) extends DissectorCR[Option[String]] {
    protected def findActualLength(input: Blob, byteOffset: Long): Int
    protected def assess(actualLength: Int): Seq[Note] = Nil

    final override def dissect(input: Blob, offset: InfoSize): AtomCR[Option[String]] = {
      val Bytes(bo) = offset

      val actualLength = findActualLength(input, bo)
      val inBuffer = ByteBuffer.wrap(input.slice(bo, bo + actualLength).toArray)
      val outBuffer = CharBuffer.allocate(256)
      val decoder = StandardCharsets.US_ASCII.newDecoder()

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
          Left(lefts.map(_.left.get).flatten.toIndexedSeq) :: groupChunks(rest)
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
        val tokenizer = new StringTokenizer(chunk, AsciiSpecialChars.keys.mkString, true)
        tokenizer.asScala.map { o =>
          val s = o.toString
          AsciiSpecialChars.getOrElse(s.head, "\"" + s + "\"")
        }.mkString(" ")
      }

      def stringifyChunk(chunk: Chunk): String =
        chunk.fold("0x" + _.map("%02x".format(_)).mkString, stringifyDecodedChunk)

      Atom(
        Bytes(length),
        new EagerContentsR(value,
          if (groupedChunks.isEmpty) "\"\""
          else groupedChunks.map(stringifyChunk).mkString(" ")),
        if (value.isEmpty) Note(Quality.Broken, "invalid encoding") +: assess(actualLength)
        else assess(actualLength))
    }
  }

  class AsciiString(length: Int) extends AsciiStringGeneric(length) {
    override protected def findActualLength(input: Blob, byteOffset: Long): Int = length
  }

  class AsciiZString(length: Int) extends AsciiStringGeneric(length) {
    override protected def findActualLength(input: Blob, byteOffset: Long): Int = {
      var actualLen = 0

      while (actualLen < length && input(byteOffset + actualLen) != 0)
        actualLen += 1

      actualLen
    }

    override protected def assess(actualLength: Int): Seq[Note] =
      if (actualLength < length) super.assess(actualLength)
      else Seq(Note(Quality.Bad, "missing NUL terminator"))
  }
}
