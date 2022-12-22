package mlscript.codegen.sourcemap

import scala.collection.mutable.Buffer
import scala.annotation.tailrec

object Base64VLQ {
  private val alphabet: List[Char] = List.from("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

  private val charIntMap: Map[Char, Int] = Map.from(alphabet.iterator.zipWithIndex)

  private val intCharMap: Map[Int, Char] =
    Map.from(alphabet.iterator.zipWithIndex.map { case (c, i) => (i, c) })

  def charToInt(ch: Char): Int =
    charIntMap.get(ch).getOrElse {
      throw Exception(s"illegal character for charToInt $ch")
    }

  def intToChar(value: Int): Char =
    intCharMap.get(value).getOrElse {
      throw Exception(s"illegal integer value for intToChar $value")
    }
  /**
    * Encode an integer to a base64 variable-length quantity string.
    *
    * @param value the integer value to be encoded
    * @return the base64 variable-length quantity string
    */
  def encode(value: Int): String =
    @tailrec
    def rec(prefix: String, num: Int): String =
      val clamped = num & 0x1f
      val shifted = num >>> 5
      val acc = prefix + intToChar(if shifted > 0 then clamped | 0x20 else clamped)
      if (shifted > 0) rec(acc, shifted) else acc
    rec("", if value < 0 then (-value << 1) | 1 else value << 1)
  
  // def decode(text: String): Int = ???
}