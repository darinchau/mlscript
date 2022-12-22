package mlscript.codegen.sourcemap

import scala.collection.mutable.{Map, Buffer}

class SetArray:
  private val _indexes: Map[String, Int] = Map.empty
  val keys: Buffer[String] = Buffer.empty
  def getKey(index: Int): String = keys(index)
  def has(key: String): Boolean = _indexes.contains(key)
  def get(key: String): Option[Int] = _indexes.get(key)
  def put(key: String): Int =
    _indexes.get(key) match
      case Some(pos) => pos
      case None =>
        val nextIndex = keys.size
        _indexes += (key -> nextIndex)
        keys += key
        nextIndex
  def pop: Unit =
    keys.lastOption match
      case None => ()
      case Some(last) =>
        keys.remove(keys.size - 1)
        _indexes.remove(last)
