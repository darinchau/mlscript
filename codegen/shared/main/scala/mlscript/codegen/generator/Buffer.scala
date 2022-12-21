package mlscript.codegen.generator

import scala.collection.mutable.{StringBuilder, ArrayDeque}
import mlscript.codegen.{Position, Location, LocationType}

private case class QueueItem(
  val char: Char,
  val repeat: Int,
  val sourcePosition: SourcePosition
)

// TODO: Find a better name.
class BufferOutput(
  val code: String,
  val decodeMap: Option[Any], // TODO: Fill the right types after they're done.
  val map: Option[SourceMap],
  val rawMappings: Option[Any] // TODO: Fill the right types after they're done.
)

class Buffer(_map: Option[SourceMap]) {
  private var _position: Position = Position(1, 0)

  private var _sourcePosition: SourcePosition = SourcePosition()

  private var _last: Char = '\u0000'
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
    fileName: Option[String]
  ): Unit = 
    _queue = _queue :+ QueueItem(char, repeat, SourcePosition(identifierName, line, column, fileName))

  private def _popQueue(): QueueItem =
    if (this._queue.isEmpty)
      throw new AssertionError("Cannot pop from empty queue")
    else {
      val item = this._queue.last
      this._queue.sliceInPlace(0, this._queue.length - 1)
      item
    }

  def get(): BufferOutput = {
    this._flush()
    val code = (this._buf.append(this._str)).toString().trim // FIXME: TrimRight
    new BufferOutput(code, None, None, None)
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
    // Drop trailing spaces when a newline is inserted.
    if (char == '\n') {
      this._queue.removeLastWhile(
        (item) => item.char == ' ' || item.char == '\t'
      )
    }

    this._pushQueue(char, 1,
      _sourcePosition.line,
      _sourcePosition.column,
      _sourcePosition.identifierName,
      _sourcePosition.fileName)
  }

  def queueIndentation(char: Char, repeat: Int): Unit =
    this._pushQueue(char, repeat, None, None, None, None)

  private def _flush(): Unit = {
    this._queue.foreach(
      (item) => this._appendChar(item.char, item.repeat, item.sourcePosition))
    this._queue.clear()
  }

  private def _appendChar(
    char: Char,
    repeat: Int,
    sourcePos: SourcePosition
  ): Unit = {
    this._last = char
    this._str =
      if (repeat > 1) this._str.append(char.toString() * repeat)
      else this._str.append(char)

    if (char != '\n') {
      // Why not `_mark(sourcePos)`?
      _mark(
        sourcePos.line,
        sourcePos.column,
        sourcePos.identifierName,
        sourcePos.fileName
      )
      _position = _position + repeat
    } else {
      _position = _position.nextLine
    }
  }

  private def _append(s: String, sourcePos: SourcePosition, maybeNewline: Boolean): Unit = {
    this._last = s.last
    this._appendCount += 1
    if (this._appendCount > 4096) { // Why this number?
      this._buf = this._buf.append(this._str)
      this._str = new StringBuilder(s)
      this._appendCount = 0
    } else {
      this._str.append(s)
    }

    if (!maybeNewline && this._map.isEmpty)
      this._position = _position + s.length
    else {
      var i = s.indexOf('\n')
      var last = 0
      var line = sourcePos.line.get // Not sure why.

      if (i != 0)
        this._mark(Some(line), sourcePos.column, sourcePos.identifierName, sourcePos.fileName)

      while (i != -1) {
        this._position = _position.nextLine

        last = i + 1

        if (last < s.length) {
          line += 1
          this._mark(Some(line), Some(0), sourcePos.identifierName, sourcePos.fileName)
        }

        i = s.indexOf('\n', last)
      }

      this._position = _position + (s.length - last)
    }
  }

  private def _mark(
    line: Option[Int],
    column: Option[Int],
    identifierName: Option[String],
    fileName: Option[String]
  ): Unit = {
    // TODO: Mark on Source Map
  }

  def removeTrailingNewline(): Unit =
    if (!_queue.isEmpty && _queue.last.char == '\n')
      _queue.dropRightInPlace(1)
  
  def removeLastSemicolon(): Unit =
    if (!_queue.isEmpty && _queue.last.char == ';')
      _queue.dropRightInPlace(1)

  def getLastChar: Char =
    if (!_queue.isEmpty) _queue.last.char else this._last
  
  def getNewlineCount: Int = 
    if (_queue.isEmpty) {
      if (this._last == '\n') 1 else 0
    }
    else {
      val count = this._queue.foldRight((0, true))((item, p) =>
        if (item.char == '\n' && p._2) (p._1 + 1, true)
        else (p._1, false))._1
      if (count == this._queue.length && this._last == '\n')
        count + 1
      else count
    }

  def endsWithCharAndNewline: Option[Char] =
    if (!_queue.isEmpty) {
      val lastCp = _queue.last
      if (lastCp.char != '\n') None
      else if (_queue.length > 1)
        Some(_queue(_queue.length - 2).char)
      else None
    }
    else None

  def hasContent: Boolean =
    !_queue.isEmpty || this._last != '\u0000'

  def exactSource(loc: Option[Location], cb: () => Unit): Unit =
    if (this._map.isEmpty) cb()
    else {
      this.source(LocationType.Start, loc)
      cb()
      this.source(LocationType.End, loc)
    }

  def source(prop: LocationType, loc: Option[Location]): Unit =
    if (!this._map.isEmpty) this._normalizePosition(prop, loc, 0, 0)

  def sourceWithOffset(
    prop: LocationType,
    loc: Option[Location],
    lineOffset: Int,
    columnOffset: Int
  ): Unit =
    if (!this._map.isEmpty) this._normalizePosition(prop, loc, lineOffset, columnOffset)

  def withSource(prop: LocationType, loc: Option[Location], cb: () => Unit): Unit =
    if (this._map.isEmpty) cb()
    else {
      this.source(prop, loc)
      cb()
    }

  private def _normalizePosition(
    prop: LocationType,
    loc: Option[Location],
    lineOffset: Int,
    columnOffset: Int
  ): Unit = {
    val pos = loc.flatMap(_(prop))
    val targetIdName: Option[String] = prop match
      case LocationType.Start => loc.flatMap(_.identifierName)
      case LocationType.End => None
    _sourcePosition = pos match
      case Some(pos) => SourcePosition(targetIdName, Some(pos.line + lineOffset), Some(pos.column + columnOffset), loc.flatMap(_.fileName))
      case None => SourcePosition(targetIdName, _sourcePosition.line, _sourcePosition.column, _sourcePosition.fileName)
  }

  def getCurrentColumn: Int = {
    var lastIndex = -1
    val len = _queue.foldLeft(0)((le, item) => {
      if (item.char == '\n') lastIndex = le
      le + item.repeat
    })

    if (lastIndex == -1) this._position.column + len
    else len - 1 - lastIndex
  }

  def getCurrentLine: Int =
    this._position.line + _queue.foldLeft(0)((count, item) =>
      if (item.char == '\n') count + 1 else count)
}
