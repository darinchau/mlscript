package mlscript.codegen.generator

import scala.collection.mutable.{StringBuilder, ArrayDeque}

class Buffer(map: Option[SourceMap]) {
  type Pos = {
    val line: Int
    val column: Int
  }

  type Loc = {
    val start: Option[Pos]
    val end: Option[Pos]
    val identifierName: Option[String]
    val filename: Option[String]
  }

  type SourcePos = {
    val identifierName: Option[String]
    val line: Option[Int]
    val column: Option[Int]
    val filename: Option[String]
  }

  type QueueItem = {
    val char: Char
    val repeat: Int
    val line: Option[Int]
    val column: Option[Int]
    val identifierName: Option[String]
    val filename: Option[String]
  }

  private var position: Pos = new {
    val line = 1
    val column = 0
  }

  private var sourcePosition: SourcePos = new {
    val identifierName = None
    val line = None
    val column = None
    val filename = None
  }

  private var last: Char = '\0'
  private var str = new StringBuilder()
  private var buf = new StringBuilder()
  private var queue = new ArrayDeque[QueueItem](16) // No need for _allocQueue. We can initialize it by using constructor.

  private def _pushQueue(
    char: Char,
    repeat: Int,
    line: Option[Int],
    column: Option[Int],
    identifierName: Option[String],
    filename: Option[String]
  ): Unit = {
    val item = new {
      val char = char
      val repeat = repeat
      val line = line
      val column = column
      val identifierName = identifierName
      val filename = filename
    }

    queue = queue :+ item
  }

  private def _popQueue(): QueueItem =
    if (this.queue.isEmpty)
      throw new AssertionError("Cannot pop from empty queue")
    else {
      val item = this.queue.last
      this.queue.sliceInPlace(0, this.queue.size - 1)
      item
    }

  private def _appendChar(
    char: Char,
    repeat: Int,
    sourcePos: SourcePos
  ): Unit = {
    this.last = char
    this.str =
      if (repeat > 1) this.str.append(char.toString() * repeat)
      else this.str.append(char)

    if (char != CharCodes.lineFeed) {
      this._mark(
        sourcePos.line,
        sourcePos.column,
        sourcePos.identifierName,
        sourcePos.filename
      )

      this.position = new {
        val line = this.position.line
        val column = this.position.column + repeat
      }
    }
    else {
      this.position = new {
        val line = this.position.line + 1
        val column = 0
      }
    }
  }

  private def _flush(): Unit = {
    this.queue.foreach(
      (item) => this._appendChar(item.char, item.repeat, item))
    this.queue.clear()
  }

  def append(s: String, maybeNewline: Boolean): Unit = {
    this._flush()
    // TODO
  }

  def appendChar(char: Char): Unit = {
    this._flush()
    this._appendChar(char, 1, this.sourcePosition)
  }

  def queue(char: Char): Unit = {

  }

  def queueIndentation(char: Char, repeat: Int): Unit =
    this._pushQueue(char, repeat, None, None, None, None)

  private def _append(s: String, sourcePos: SourcePos, maybeNewline: Boolean): Unit = {

  }

  private def _mark(
    line: Option[Int],
    column: Option[Int],
    identifierName: Option[String],
    filename: Option[String]
  ): Unit = {
    // TODO: Mark on Source Map
  }
}
