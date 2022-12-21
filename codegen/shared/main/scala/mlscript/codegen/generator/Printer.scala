package mlscript.codegen.generator

import scala.util.matching.Regex
import scala.collection.mutable.{ArrayBuffer, HashSet}
import mlscript.codegen.babel._

class Printer(format: Format, map: SourceMap) {
  private type NewlineState = {
    var printed: Boolean
  }

  private val PURE_ANNOTATION_RE = "^\\s*[@#]__PURE__\\s*$".r

  private var indentLevel: Int = 0
  private val _buf = new Buffer(map)
  private var _noLineTerminator = false
  private var _endsWithWord = false
  private var _endWithInteger = false
  private var _indentChar = '\0'
  private var _indentRepeat = 0
  private var _endsWithInteger = false
  private var _endWithInnerRaw = false
  private var _indentInnerComments = true
  private var _parenPushNewlineState: Option[NewlineState] = None
  private var _printAuxAfterOnNextUserNode = false
  private var _printStack = new ArrayBuffer[BaseNode]()
  private var _printedComments = new HashSet[BaseComment]()

  def generate(ast: BaseNode) = {
    this.print(Some(ast))
    this._maybeAddAuxComment()

    this._buf.get()
  }

  def indent(): Unit =
    if (!format.compact && !format.concise) this.indentLevel += 1

  def dedent(): Unit =
    if (!format.compact && !format.concise) this.indentLevel -= 1

  def semicolon(force: Boolean = false): Unit = {
    this._maybeAddAuxComment()
    if (force) this._appendChar(CharCode.semicolon)
    else this._queue(CharCode.semicolon)

    this._noLineTerminator = false
  }

  def rightBrace = {
    if (this.format.minified) this._buf.removeLastSemicolon()
    this.token("}")
  }

  def space(force: Boolean = false): Unit =
    if (!format.compact) {
      if (force) this._space
      else if (_buf.hasContent) {
        val lastCp = getLastChar()
        if (lastCp != CharCodes.space && lastCp != CharCodes.lineFeed)
          this._space
      }
    }

  def word(str: String, noLineTerminatorAfter: Boolean = false) = {
    this._maybePrintInnerComments()
    if (this._endsWithWord || (str.charAt(0) == CharCode.slash && this.endsWith(CharCode.slash)))
      this.space()

    this._maybeAddAuxComment()
    this._append(str, false)

    this._endsWithWord = true
    this._noLineTerminator = noLineTerminatorAfter
  }

  def number(str: String): Unit = {
    // TODO:
    // @see printer.ts line 220
  }

  def token(str: String, maybeNewline: Boolean = false): Unit = {
    this._maybePrintInnerComments()

    val lastChar = this.getLastChar
    val strFirst = str.charAt(0)
    
    if ((lastChar == CharCodes.exclamationMark && str === "--") ||
        (strFirst == CharCodes.plusSign && lastChar == CharCodes.plusSign) ||
        (strFirst == CharCodes.dash && lastChar == CharCodes.dash) ||
        (strFirst == CharCodes.dot && this._endWithInteger))
      this._space

    this._maybeAddAuxComment()
    this._append(str, maybeNewline)
    this._noLineTerminator = false
  }

  def tokenChar(char: Char): Unit = {
    // TODO:
    // @see printer.ts line 260
  }

  def newline(i: Int = 1, force: Boolean = false): Unit =
    if (i > 0) {
      if (!force && !format.retainLines && !format.compact && format.concise)
        this.space()
      else if (force) {
        var ii: Int
        for (ii <- 0 until (if (i > 2) 2 else i) - _buf.getNewlineCount)
          this._newline()
      }
    }

  def endsWith(char: Char): Boolean =
    this.getLastChar == char

  def getLastChar: Char = this._buf.getLastChar

  def endsWithCharAndNewline =
    this._buf.endsWithCharAndNewline

  def removeTrailingNewline = 
    this._buf.removeTrailingNewline()

  def exactSource() = {
    // TODO:
    // @see printer.ts line 326
  }

  def source(prop: LocType, loc: Option[Loc]): Unit =
    if (!loc.isEmpty) {
      this._catchUp(prop, loc)
      this._buf.source(prop, loc)
    }
  
  def sourceWithOffset() = {
    // TODO:
    // @see printer.ts line 342
  }

  def withSource() = {
    // TODO:
    // @see printer.ts line 355
  }

  private def _space = this._queue(CharCodes.space)
  private def _newline = this._queue(CharCodes.lineFeed)

  private def _append(str: String, maybeNewline: Boolean): Unit = {
    this._maybeAddParen(str)
    this._maybeIndent(str.charAt(0))

    this._buf.append(str, maybeNewline)

    this._endsWithWord = false
    this._endsWithInteger = false
  }

  private def _appendChar(char: Char): Unit = {
    this._maybeAddParenChar(char)
    this._maybeIndent(char)

    this._buf.appendChar(char)

    this._endsWithWord = false
    this._endWithInteger = false
  }

  private def _queue(char: Char): Unit = {
    this._maybeAddParenChar(char)
    this._maybeIndent(char)

    this._buf.queue(char)

    this._endsWithWord = false
    this._endWithInteger = false
  }

  private def _maybeIndent(firstChar: Char): Unit =
    if (this.indentLevel > 0 &&
        firstChar != CharCodes.lineFeed &&
        this.endsWith(CharCodes.lineFeed))
      this._buf.queueIndentation(this._indentChar, this._getIndent)

  private def _shouldIndent(firstChar: Char) =
    this.indentLevel > 0 && firstChar != CharCode.lineFeed && this.endsWith(CharCode.lineFeed)

  private def _maybeAddParenChar(char: Char): Unit =
    if (!_parenPushNewlineState.isEmpty && char != CharCodes.space) {
      if (char != CharCodes.lineFeed)
        this._parenPushNewlineState = None
      else {
        this.token("(")
        this.indent()
        this._parenPushNewlineState.get.printed = true
      }
    }

  private def _maybeAddParen(str: String): Unit =
    if (!_parenPushNewlineState.isEmpty) {
      val len = str.length()
      val noSpaceIndex = str.indexWhere((c) => c != CharCodes.space)
      if (noSpaceIndex < len) {
        val char = str.charAt(noSpaceIndex)
        if (char == CharCodes.lineFeed) {
          if (char != CharCodes.slash || noSpaceIndex + 1 == len)
            this._parenPushNewlineState = None
          else {
            val charPost = str.charAt(noSpaceIndex + 1)
            if (charPost == CharCodes.asterisk) {
              (PURE_ANNOTATION_RE findFirstIn str.slice(noSpaceIndex + 2, len - 2)) match {
                case None => {
                  this.token("(")
                  this.indent()
                  this._parenPushNewlineState.get.printed = true
                }
                case _ => ()
              }
            }
            else if (charPost != CharCodes.slash) {
              this._parenPushNewlineState = None
            }
            else {
              this.token("(")
              this.indent()
              this._parenPushNewlineState.get.printed = true
            }
          }
        }
        else {
          this.token("(")
          this.indent()
          this._parenPushNewlineState.get.printed = true
        }
      }
    }

  def catchUp(line: Int) = {
    // TODO:
    // @see printer.ts line 517
  }

  private def _catchUp(prop: LocType, loc: Option[Loc]): Unit =
    if (this.format.retainLines && !loc.isEmpty) {
      val pos = prop match {
        case "start" => loc.get.start
        case _ => loc.get.end
      }

      var count: Int
      for (count <- 0 until (pos.line - this._buf.getCurrentLine)) {
        this._newline()
      }
    }

  private def _getIndent: Int =
    this._indentRepeat * this.indentLevel

  def printTerminatorless() = {
    // TODO:
    // @see printer.ts line 550
  }

  def print(
    node: Option[BaseNode],
    parent: Option[BaseNode] = None,
    noLineTerminatorAfter: Boolean = false,
    trailingCommentsLineOffset: Int = 0,
    forceParens: Boolean = false
  ) = if (!node.isEmpty) {
    this._endWithInnerRaw = false
    // TODO: Finish print
    // @see printer.ts line 610
  }

  private def _maybeAddAuxComment(enteredPositionlessNode: Boolean): Unit =
    if (enteredPositionlessNode) this._printAuxBeforeComment()
    else this._printAuxAfterComment()

  private def _printAuxBeforeComment(): Unit =
    if (!this._printAuxAfterOnNextUserNode) {
      this._printAuxAfterOnNextUserNode = true
      if (!this.format.auxiliaryCommentBefore.isEmpty) {
        // FIXME: Use correct comment type
        // @see printer.ts line 691
        // this._printComment(new BaseComment, COMMENT_SKIP_NEWLINE.DEFAULT)
      }
    }

  private def _printAuxAfterComment(): Unit =
    if (!this._printAuxAfterOnNextUserNode) {
      this._printAuxAfterOnNextUserNode = true
      if (!this.format.auxiliaryCommentAfter.isEmpty) {
        // FIXME: Use correct comment type
        // @see printer.ts line 707
        // this._printComment(new BaseComment, COMMENT_SKIP_NEWLINE.DEFAULT)
      }
    }

  def getPossibleRaw = {
    // TODO:
    // @see printer.ts line 717
  }

  def printJoin() = {
    // TODO:
    // @see printer.ts line 738
  }

  def printAndIndentOnComments() = {
    // TODO:
    // @see printer.ts line 782
  }

  def printBlock() = {
    // TODO:
    // @see printer.ts line 789
  }

  private def _printTrailingComments() = {
    // TODO:
    // @see printer.ts line 799
  }

  private def _printLeadingComments() = {
    // TODO:
    // @see printer.ts line 824
  }

  private def _maybePrintInnerComments(): Unit = {
    if (this._endWithInnerRaw) this.printInnerComments()

    this._endWithInnerRaw = true
    this._indentInnerComments = true
  }

  def printInnerComments(): Unit = {
    val node = _printStack.last
    val comments: Option[Array[BaseComment]] = node.innerComments // FIXME: Get correct node
    if (!comments.isEmpty & comments.get.length > 0) {
      val hasSpace = this.endsWith(CharCode.space)
      val printedCommentsCount = this._printedComments.size
      if (this._indentInnerComments) this.indent()

      this._printComments(COMMENT_TYPE.INNER ,comments.get, node)
      if (hasSpace && printedCommentsCount != this._printedComments.size)
        this.space()
      if (this._indentInnerComments) this.dedent()
    }
  }

  def noIndentInneerCommentsHere() = this._indentInnerComments = false

  def printSequence() = {
    // TODO:
    // @see printer.ts line 856
  }

  def printList() = {
    // TODO:
    // @see printer.ts line 865
  }

  private def _printNewline() = {
    // TODO:
    // @see printer.ts line 873
  }

  private def _shouldPrintComment() = {
    // TODO:
    // @see printer.ts line 921
  }

  private def _printComment(comment: BaseComment, skipNewLines: COMMENT_SKIP_NEWLINE) = {
    val isBlockComment = comment.tp == "CommentBlock"
    val printNewLines =
      isBlockComment && skipNewLines != COMMENT_SKIP_NEWLINE.ALL && !this._noLineTerminator
    
    if (printNewLines && this._buf.hasContent && skipNewLines != COMMENT_SKIP_NEWLINE.LEADING)
      this.newline(1)

    val value: String = if (isBlockComment) {
      if (this.format.adjustMultilineComment) {
        // FIXME: Use correct comment type
        // @see printer.ts line 973
      }
      else s"/*${comment.value}*/"
    }
    else if (!_noLineTerminator) s"//${comment.value}"
    else s"/*${comment.value}*/"

    if (this.endsWith(CharCodes.slash)) this._space

    this.source("start", comment.loc)
    this._append(value, isBlockComment)

    if (!isBlockComment && !_noLineTerminator) this.newline(1, true)
    if (printNewLines && skipNewLines != COMMENT_SKIP_NEWLINE.TRAILING)
      this.newline(1)
  }

  private def _printComments(
    tp: COMMENT_TYPE,
    comments: Array[BaseComment],
    node: BaseNode,
    parent: Option[BaseNode] = None,
    lineOffset: Int = 0
  ) = {
    // TODO: Finish print comments
    // @see printer.ts line 1016
  }
}
