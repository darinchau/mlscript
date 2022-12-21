package mlscript.codegen.generator

import scala.collection.mutable.{StringBuilder, ArrayDeque}
import CharCodes.*

class Buffer(_map: Option[SourceMap]) {
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

  private var _position: Pos = new {
    val line = 1
    val column = 0
  }

  private var _sourcePosition: SourcePos = new {
    val identifierName = None
    val line = None
    val column = None
    val filename = None
  }

  private var _last: Char = '\0'
  private var _str = new StringBuilder()
  private var _buf = new StringBuilder()
  private var _queue = new ArrayDeque[QueueItem](16) // No need for _allocQueue. We can initialize it by using constructor.
  private var _appendCount = 0

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

    _queue = _queue :+ item
  }

  private def _popQueue(): QueueItem =
    if (this._queue.isEmpty)
      throw new AssertionError("Cannot pop from empty queue")
    else {
      val item = this._queue.last
      this._queue.sliceInPlace(0, this._queue.length - 1)
      item
    }

  def get() = {
    this._flush()
    new {
      val code = (this._buf.append(this._str)).toString().trim // FIXME: TrimRight
      val decodedMap = None // TODO: map?.getDecoded()
      
      var map = _map
      var rawMappings = map match {
        case Some(m) => Some(m.getRawMappings())
        case _ => None
      }
    }
  }

  def append(s: String, maybeNewline: Boolean): Unit = {
    this._flush()
    this._append(s, this._sourcePosition, maybeNewline)
  }

  def appendChar(char: Char): Unit = {
    this._flush()
    this._appendChar(char, 1, this._sourcePosition)
  }

  def queue(char: Char): Unit = {
    if (char == CharCodes.lineFeed) {
      this._queue = this._queue.removeLastWhile(
        (item) =>
          (item.char == CharCodes.space || item.char == CharCodes.tab))
    }

    this._pushQueue(char, 1,
      _sourcePosition.line,
      _sourcePosition.column,
      _sourcePosition.identifierName,
      _sourcePosition.filename)
  }

  def queueIndentation(char: Char, repeat: Int): Unit =
    this._pushQueue(char, repeat, None, None, None, None)

  private def _flush(): Unit = {
    this._queue.foreach(
      (item) => this._appendChar(item.char, item.repeat, item))
    this._queue.clear()
  }

  private def _appendChar(
    char: Char,
    repeat: Int,
    sourcePos: SourcePos
  ): Unit = {
    this._last = char
    this._str =
      if (repeat > 1) this._str.append(char.toString() * repeat)
      else this._str.append(char)

    if (char != CharCodes.lineFeed) {
      this._mark(
        sourcePos.line,
        sourcePos.column,
        sourcePos.identifierName,
        sourcePos.filename
      )

      this._position = new {
        val line = this._position.line
        val column = this._position.column + repeat
      }
    }
    else {
      this._position = new {
        val line = this._position.line + 1
        val column = 0
      }
    }
  }

  private def _append(s: String, sourcePos: SourcePos, maybeNewline: Boolean): Unit = {
    val len = s.length()

    this._last = s.last
    this._appendCount += 1
    if (this._appendCount > 4096) {
      this._buf = this._buf.append(this._str)
      this._str = s
      this._appendCount = 0
    }
    else {
      this._str.append(s)
    }

    if (!maybeNewline && this._map.isEmpty)
      this._position = new {
        val line = this._position.line
        val column = this._position.column + len
      }
    else {
      var i = s.indexOf(CharCodes.lineFeed)
      var last = 0
      var line = sourcePos.line

      if (i != 0)
        this._mark(line, sourcePos.column, sourcePos.identifierName, sourcePos.filename)

      while (i != -1) {
        this._position = new {
          val line = this._position.line + 1
          val column = 0
        }

        last = i + 1

        if (last < len) {
          line += 1
          this._mark(line, 0, sourcePos.identifierName, sourcePosfilename)
        }

        i = s.indexOf(CharCodes.lineFeed, last)
      }

      this._position = new {
        val line = this._position.line
        val column = this._position.column + len - last
      }
    }
  }

  private def _mark(
    line: Option[Int],
    column: Option[Int],
    identifierName: Option[String],
    filename: Option[String]
  ): Unit = {
    // TODO: Mark on Source Map
  }

  def removeTrailingNewline(): Unit =
    if (!_queue.isEmpty && _queue.last == CharCodes.lineFeed)
      _queue.dropRightInPlace(1)
  
  def removeLastSemicolon(): Unit =
    if (!_queue.isEmpty && _queue.last == CharCodes.semicolon)
      _queue.dropRightInPlace(1)

  def getLastChar: Char =
    if (!_queue.isEmpty) _queue.last.char else this._last
  
  def getNewlineCount: Int = 
    if (_queue.isEmpty) {
      if (this._last == CharCodes.lineFeed) 1 else 0
    }
    else {
      val count = this._queue.foldRight((0, true))((item, p) =>
        if (item.char == CharCodes.lineFeed && p._2) (p._1 + 1, true)
        else (p._1, false))._1
      if (count == this._queue.length && this._last === CharCodes.lineFeed)
        count + 1
      else count
    }

  def endsWithCharAndNewline: Option[Char] =
    if (!_queue.isEmpty) {
      val lastCp = _queue.last
      if (lastCp != CharCodes.lineFeed) None
      else if (_queue.length > 1)
        Some(_queue(_queue.length - 2).char)
      else None
    }
    else None

  def hasContent: Boolean =
    !_queue.isEmpty || this._last != '\0'

  def exactSource(loc: Option[Loc], cb: () => Unit): Unit =
    if (this._map.isEmpty) cb()
    else {
      this.source("start", loc)
      cb()
      this.source("end", loc)
    }

  def source(prop: LocType, loc: Option[Loc]): Unit =
    if (!this._map.isEmpty) this._normalizePosition(prop, loc, 0, 0)

  def sourceWithOffset(
    prop: LocType,
    loc: Option[Loc],
    lineOffset: Int,
    columnOffset: Int
  ): Unit =
    if (!this._map.isEmpty) this._normalizePosition(prop, loc, lineOffset, columnOffset)

  def withSource(prop: LocType, loc: Option[Loc], cb: () => Unit): Unit =
    if (this._map.isEmpty) cb()
    else {
      this.source(prop, loc)
      cb()
    }

  private def _normalizePosition(
    prop: LocType,
    loc: Option[Loc],
    lineOffset: Int,
    columnOffset: Int
  ): Unit = {
    val pos = prop match {
      case Left(_) => loc.start
      case _ => loc.end
    }

    val targetIdName: Option[String] = prop match {
      case Left(_) if (!loc.identifierName.isEmpty) => loc.identifierName
      case _ => None
    }

    val hasPos = !pos.isEmpty
    val targetLine =
      if (hasPos) Some(pos.get().line + lineOffset) else _sourcePosition.line
    val targetColumn =
      if (hasPos) Some(pos.get().column + columnOffset) else _sourcePosition.column
    val targetFilename =
      if (hasPos) loc.get().filename else _sourcePosition.filename

    this._sourcePosition = new {
      val identifierName = targetIdName
      val line = targetLine
      val column = targetColumn
      val filename = targetFilename
    }
  }

  def getCurrentColumn: Int = {
    var lastIndex = -1
    val len = _queue.foldLeft(0)((le, item) => {
      if (item.char == CharCodes.lineFeed) lastIndex = le
      le + item.repeat
    })

    if (lastIndex == -1) this._position.column + len
    else len - 1 - lastIndex
  }

  def getCurrentLine: Int =
    this._position.line + _queue.foldLeft(0)((count, item) =>
      if (item.char == CharCodes.lineFeed) count + 1 else count)
}
