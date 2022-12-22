package mlscript.codegen.generator

import scala.util.matching.Regex
import scala.collection.mutable.{ArrayBuffer, HashSet}
import mlscript.codegen.babel._
import mlscript.codegen.{Position, Location, LocationType}

class NewLineState(var printed: Boolean)

class Printer(format: Format, map: SourceMapBuilder) {
  private val PURE_ANNOTATION_RE = "^\\s*[@#]__PURE__\\s*$".r

  private var indentLevel: Int = 0
  private val _buf = new Buffer(Some(map))
  private var _noLineTerminator = false
  private var _endsWithWord = false
  private var _endWithInteger = false
  private var _indentChar = '\u0000'
  private var _indentRepeat = 0
  private var _endsWithInteger = false
  private var _endWithInnerRaw = false
  private var _indentInnerComments = true
  private var _parenPushNewlineState: Option[NewLineState] = None
  private var _printAuxAfterOnNextUserNode = false
  private var _printStack = new ArrayBuffer[BaseNode]()
  private var _printedComments = new HashSet[BaseComment]()

  def generate(ast: BaseNode) = {
    print(Some(ast))
    _maybeAddAuxComment()

    _buf.get()
  }

  def indent(): Unit =
    if (!format.compact && !format.concise) indentLevel += 1

  def dedent(): Unit =
    if (!format.compact && !format.concise) indentLevel -= 1

  def semicolon(force: Boolean = false): Unit = {
    _maybeAddAuxComment()
    if (force) _appendChar(';')
    else _queue(';')

    _noLineTerminator = false
  }

  def rightBrace = {
    if (format.minified) _buf.removeLastSemicolon()
    token("}")
  }

  def space(force: Boolean = false): Unit =
    if (!format.compact) {
      if (force) _space()
      else if (_buf.hasContent) {
        val lastCp = getLastChar()
        if (lastCp != ' ' && lastCp != '\n')
          _space()
      }
    }

  def word(str: String, noLineTerminatorAfter: Boolean = false) = {
    _maybePrintInnerComments()
    if (_endsWithWord || (str.charAt(0) == '/' && endsWith('/')))
      space()

    _maybeAddAuxComment()
    _append(str, false)

    _endsWithWord = true
    _noLineTerminator = noLineTerminatorAfter
  }

  // TODO:
  // @see printer.ts line 220
  def number(str: String): Unit = ???

  def token(str: String, maybeNewline: Boolean = false): Unit = {
    _maybePrintInnerComments()

    val lastChar = getLastChar()
    val strFirst = str.charAt(0)
    
    if ((lastChar == '!' && str == "--") ||
        (strFirst == '+' && lastChar == '+') ||
        (strFirst == '-' && lastChar == '-') ||
        (strFirst == '.' && _endWithInteger))
      _space()

    _maybeAddAuxComment()
    _append(str, maybeNewline)
    _noLineTerminator = false
  }

  // TODO:
  // @see printer.ts line 260
  def tokenChar(char: Char): Unit = ???

  def newline(i: Int = 1, force: Boolean = false): Unit =
    if (i > 0) {
      if (!force && !format.retainLines && !format.compact && format.concise)
        space()
      else if (force) {
        for (ii <- 0 until (if (i > 2) 2 else i) - _buf.getNewlineCount)
          _newline()
      }
    }

  def endsWith(char: Char): Boolean =
    getLastChar() == char

  def getLastChar(): Char = _buf.getLastChar

  def endsWithCharAndNewline =
    _buf.endsWithCharAndNewline

  def removeTrailingNewline = 
    _buf.removeTrailingNewline()

  // TODO:
  // @see printer.ts line 326
  def exactSource() = ???

  def source(prop: LocationType, loc: Option[Location]): Unit =
    if (!loc.isEmpty) {
      _catchUp(prop, loc)
      _buf.source(prop, loc)
    }
  
  // TODO:
  // @see printer.ts line 342
  def sourceWithOffset() = ???

  // TODO:
  // @see printer.ts line 355
  def withSource() = ???

  private def _space() = _queue(' ')
  private def _newline() = _queue('\n')

  private def _append(str: String, maybeNewline: Boolean): Unit = {
    _maybeAddParen(str)
    _maybeIndent(str.charAt(0))

    _buf.append(str, maybeNewline)

    _endsWithWord = false
    _endsWithInteger = false
  }

  private def _appendChar(char: Char): Unit = {
    _maybeAddParenChar(char)
    _maybeIndent(char)

    _buf.appendChar(char)

    _endsWithWord = false
    _endWithInteger = false
  }

  private def _queue(char: Char): Unit = {
    _maybeAddParenChar(char)
    _maybeIndent(char)

    _buf.queue(char)

    _endsWithWord = false
    _endWithInteger = false
  }

  private def _maybeIndent(firstChar: Char): Unit =
    if (indentLevel > 0 &&
        firstChar != '\n' &&
        endsWith('\n'))
      _buf.queueIndentation(_indentChar, _getIndent)

  private def _shouldIndent(firstChar: Char) =
    indentLevel > 0 && firstChar != '\n' && endsWith('\n')

  private def _maybeAddParenChar(char: Char): Unit =
    if (!_parenPushNewlineState.isEmpty && char != ' ') {
      if (char != '\n')
        _parenPushNewlineState = None
      else {
        token("(")
        indent()
        _parenPushNewlineState.get.printed = true
      }
    }

  private def _maybeAddParen(str: String): Unit =
    if (!_parenPushNewlineState.isEmpty) {
      val len = str.length()
      val noSpaceIndex = str.indexWhere((c) => c != ' ')
      if (noSpaceIndex < len) {
        val char = str.charAt(noSpaceIndex)
        if (char == '\n') {
          if (char != '/' || noSpaceIndex + 1 == len)
            _parenPushNewlineState = None
          else {
            val charPost = str.charAt(noSpaceIndex + 1)
            if (charPost == '*') {
              (PURE_ANNOTATION_RE findFirstIn str.slice(noSpaceIndex + 2, len - 2)) match {
                case None => {
                  token("(")
                  indent()
                  _parenPushNewlineState.get.printed = true
                }
                case _ => ()
              }
            }
            else if (charPost != '/') {
              _parenPushNewlineState = None
            }
            else {
              token("(")
              indent()
              _parenPushNewlineState.get.printed = true
            }
          }
        }
        else {
          token("(")
          indent()
          _parenPushNewlineState.get.printed = true
        }
      }
    }

  // TODO:
  // @see printer.ts line 517
  def catchUp(line: Int) = ???

  private def _catchUp(prop: LocationType, optLoc: Option[Location]): Unit =
    if (format.retainLines)
      optLoc match
        case Some(loc) => 
          val pos = loc(prop)
          for (count <- 0 until (pos.get.line - _buf.getCurrentLine)) {
            _newline()
          }
        case None => ()

  private def _getIndent: Int =
    _indentRepeat * indentLevel

  // TODO:
  // @see printer.ts line 550
  def printTerminatorless() = ???

  def print(
    node: Option[BaseNode],
    parent: Option[BaseNode] = None,
    noLineTerminatorAfter: Boolean = false,
    trailingCommentsLineOffset: Int = 0,
    forceParens: Boolean = false
  ) = if (!node.isEmpty) {
    _endWithInnerRaw = false
    // TODO: Finish print
    // @see printer.ts line 610
    ???
  }

  private def _maybeAddAuxComment(enteredPositionlessNode: Boolean = false): Unit =
    if (enteredPositionlessNode) _printAuxBeforeComment()
    else _printAuxAfterComment()

  private def _printAuxBeforeComment(): Unit =
    if (!_printAuxAfterOnNextUserNode) {
      _printAuxAfterOnNextUserNode = true
      if (!format.auxiliaryCommentBefore.isEmpty) {
        // FIXME: Use correct comment type
        // @see printer.ts line 691
        // _printComment(new BaseComment, CommentSkipNewLine.Default)
        ???
      }
    }

  private def _printAuxAfterComment(): Unit =
    if (!_printAuxAfterOnNextUserNode) {
      _printAuxAfterOnNextUserNode = true
      if (!format.auxiliaryCommentAfter.isEmpty) {
        // FIXME: Use correct comment type
        // @see printer.ts line 707
        // _printComment(new BaseComment, CommentSkipNewLine.Default)
        ???
      }
    }

  // TODO:
  // @see printer.ts line 717
  def getPossibleRaw = ???

  // TODO:
  // @see printer.ts line 738
  def printJoin() = ???

  // TODO:
  // @see printer.ts line 782
  def printAndIndentOnComments() = ???

  // TODO:
  // @see printer.ts line 789
  def printBlock() = ???

  // TODO:
  // @see printer.ts line 799
  private def _printTrailingComments() = ???

  // TODO:
  // @see printer.ts line 824
  private def _printLeadingComments() = ???

  private def _maybePrintInnerComments(): Unit = {
    if (_endWithInnerRaw) printInnerComments()

    _endWithInnerRaw = true
    _indentInnerComments = true
  }

  def printInnerComments(): Unit = {
    val node = _printStack.last
    val comments: Option[Array[BaseComment]] =
      // node.innerComments // FIXME: Get correct node
      ???
    if (!comments.isEmpty & comments.get.length > 0) {
      val hasSpace = endsWith(' ')
      val printedCommentsCount = _printedComments.size
      if (_indentInnerComments) indent()

      _printComments(CommentType.Inner ,comments.get, node)
      if (hasSpace && printedCommentsCount != _printedComments.size)
        space()
      if (_indentInnerComments) dedent()
    }
  }

  def noIndentInneerCommentsHere() = _indentInnerComments = false

  // TODO:
  // @see printer.ts line 856
  def printSequence() = ???

  // TODO:
  // @see printer.ts line 865
  def printList() = ???

  // TODO:
  // @see printer.ts line 873
  private def _printNewline() = ???

  // TODO:
  // @see printer.ts line 921
  private def _shouldPrintComment() = ???

  private def _printComment(comment: BaseComment, skipNewLines: CommentSkipNewLine) = {
    val isBlockComment = comment.kind == CommentKind.Block
    val printNewLines =
      isBlockComment && skipNewLines != CommentSkipNewLine.All && !_noLineTerminator
    
    if (printNewLines && _buf.hasContent && skipNewLines != CommentSkipNewLine.Leading)
      newline(1)

    val value: String = if (isBlockComment) {
      if (format.adjustMultilineComment) {
        // FIXME: Use correct comment type
        // @see printer.ts line 973
        ???
      }
      else s"/*${comment.value}*/"
    }
    else if (!_noLineTerminator) s"//${comment.value}"
    else s"/*${comment.value}*/"

    if (endsWith('/')) _space()

    source(LocationType.Start, comment.loc)
    _append(value, isBlockComment)

    if (!isBlockComment && !_noLineTerminator) newline(1, true)
    if (printNewLines && skipNewLines != CommentSkipNewLine.Trailing)
      newline(1)
  }

  private def _printComments(
    ty: CommentType,
    comments: Array[BaseComment],
    node: BaseNode,
    parent: Option[BaseNode] = None,
    lineOffset: Int = 0
  ) =
    // TODO: Finish print comments
    // @see printer.ts line 1016
    ???
}
