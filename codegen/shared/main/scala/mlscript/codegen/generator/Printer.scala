package mlscript.codegen.generator

import scala.util.matching.Regex
import scala.collection.mutable.{ArrayBuffer, HashSet}
import mlscript.codegen.{Position, Location, LocationType}
import mlscript.codegen.ast.{Comment, CommentKind, CommentType, Node}

class NewLineState(var printed: Boolean)

trait AddNewlineOptions(val nextNodeStartLine: Int) {
  def addNewlines(leading: Boolean, node: Node): Int
}

trait PrintSequenceOptions(
  val statement: Option[Boolean],
  val indent: Option[Boolean],
  val trailingCommentsLineOffset: Option[Int]
) // TODO: extends Partial<AddNewlinesOptions>

trait printListOptions(
  val separator: Option[(Printer) => Unit],
  val iterator: Option[(Node, Int) => Unit],
  val statement: Option[Boolean],
  val indent: Option[Boolean]
)

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
  private var _printStack = new ArrayBuffer[Node]()
  private var _printedComments = new HashSet[Comment]()
  private var _insideAux = false
  private var _lastCommentLine = 0

  def generate(ast: Node): BufferOutput = {
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

  def rightBrace: Unit = {
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

  def word(str: String, noLineTerminatorAfter: Boolean = false): Unit = {
    _maybePrintInnerComments()
    if (_endsWithWord || (str.charAt(0) == '/' && endsWith('/')))
      space()

    _maybeAddAuxComment()
    _append(str, false)

    _endsWithWord = true
    _noLineTerminator = noLineTerminatorAfter
  }

  def number(str: String): Unit = {
    word(str)

    // TODO: Add reg test
    _endsWithInteger =
      (try { java.lang.Double.parseDouble(str); true } catch {case _ => false}) &&
      str.last != '.'
  }

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

  def tokenChar(char: Char): Unit = {
    _maybePrintInnerComments()
    val lastChar = getLastChar()

    if ((char == '+' && lastChar == '+') ||
        (char == '-' && lastChar == '-') ||
        (char == '.' && lastChar == '.'))
      _space()

    _maybeAddAuxComment()
    _appendChar(char)
    _noLineTerminator = false
  }

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

  def endsWithCharAndNewline: Option[Char] =
    _buf.endsWithCharAndNewline

  def removeTrailingNewline: Unit = 
    _buf.removeTrailingNewline()

  def exactSource(loc: Option[Location], cb: ()=>Unit): Unit = {
    if (loc.isEmpty) cb()
    else {
      _catchUp(LocationType.Start, loc)
      _buf.exactSource(loc, cb)
    }
  }

  def source(prop: LocationType, loc: Option[Location]): Unit =
    if (!loc.isEmpty) {
      _catchUp(prop, loc)
      _buf.source(prop, loc)
    }

  def sourceWithOffset(
    prop: LocationType, 
    loc: Option[Location],
    lineOffset: Int,
    columnOffset: Int
  ): Unit = if (!loc.isEmpty) {
    _catchUp(prop, loc)
    _buf.sourceWithOffset(prop, loc, lineOffset, columnOffset)
  }

  def withSource(
    prop: LocationType, 
    loc: Option[Location],
    cb: ()=>Unit
  ): Unit = if (loc.isEmpty) cb()
      else {
        _catchUp(prop, loc)
        _buf.withSource(prop, loc, cb)
      }

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

  def catchUp(line: Int): Unit =
    if (format.retainLines && line > _buf.getCurrentLine) {
      _newline()
      catchUp(line)
    }

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

  def printTerminatorless(node: Node, parent: Node, isLabel: Boolean): Unit =
    if (isLabel) {
      _noLineTerminator = true
      print(Some(node), Some(parent))
    }
    else {
      val terminatorState = new NewLineState(false)
      _parenPushNewlineState = Some(terminatorState)
      print(Some(node), Some(parent))

      if (terminatorState.printed) {
        dedent()
        newline()
        token(")")
      }
    }

  def print(
    node: Option[Node],
    parent: Option[Node] = None,
    noLineTerminatorAfter: Boolean = false,
    trailingCommentsLineOffset: Int = 0,
    forceParens: Boolean = false
  ): Unit = if (!node.isEmpty) {
    _endWithInnerRaw = false
    val oldConcise = format.concise
    // if (node._compact) format.concise = true
    // TODO: Get print method
    
    _printStack = _printStack :+ (node.get)
    val oldAux = _insideAux
    // TODO: _insideAux = node.loc.isEmpty
    _maybeAddAuxComment(oldAux && _insideAux)

    val shouldPrintParens: Boolean =
      if (forceParens) true
      else if (format.retainFunctionParens) true // TODO: nodeType
      else Printer.needsParens(node, parent, Some(_printStack.toArray))

    if (shouldPrintParens) {
      token("(")
      _endWithInnerRaw = false
    }

    _lastCommentLine = 0
    _printLeadingComments(node, parent)

    val loc = LocationType.Start // TODO: nodeType === "Program" || nodeType === "File" ? null : node.loc;
    // TODO: exactSource(loc)

    if (shouldPrintParens) {
      _printTrailingComments(node, parent)
      token(")")
      _noLineTerminator = noLineTerminatorAfter
    }
    else if (noLineTerminatorAfter && !_noLineTerminator) {
      _noLineTerminator = true
      _printTrailingComments(node, parent)
    }
    else {
      _printTrailingComments(node, parent, trailingCommentsLineOffset)
    }

    _printStack.dropRightInPlace(1)
    format.concise = oldConcise
    _insideAux = oldAux
    _endWithInnerRaw = false
  }

  private def _maybeAddAuxComment(enteredPositionlessNode: Boolean = false): Unit =
    if (enteredPositionlessNode) _printAuxBeforeComment()
    else _printAuxAfterComment()

  private def _printAuxBeforeComment(): Unit =
    if (!_printAuxAfterOnNextUserNode) {
      _printAuxAfterOnNextUserNode = true
      if (!format.auxiliaryCommentBefore.isEmpty) {
        // TODO: Use correct comment type
        // @see printer.ts line 691
        // _printComment(new BaseComment, CommentSkipNewLine.Default)
        ???
      }
    }

  private def _printAuxAfterComment(): Unit =
    if (!_printAuxAfterOnNextUserNode) {
      _printAuxAfterOnNextUserNode = true
      if (!format.auxiliaryCommentAfter.isEmpty) {
        // TODO: Use correct comment type
        // @see printer.ts line 707
        // _printComment(new BaseComment, CommentSkipNewLine.Default)
        ???
      }
    }

  def getPossibleRaw(node: Node): Option[String] = {
    // TODO: val extra = node.extra
    ???
  }

  // TODO:
  // @see printer.ts line 738
  def printJoin(): Unit = ???

  def printAndIndentOnComments(node: Node, parent: Node) = {
    val needIndent: Boolean = ??? // TODO: node.leadingComments && node.leadingComments.length > 0
    if (needIndent) indent()
    print(Some(node), Some(parent))
    if (needIndent) dedent()
  }

  // TODO:
  // @see printer.ts line 789
  def printBlock(): Unit = ???

  private def _printTrailingComments(node: Option[Node], parent: Option[Node], lineOffset: Int = 0) = {
    // TODO: val comments = node.leadingComments
  }

  private def _printLeadingComments(node: Option[Node], parent: Option[Node]) = {
    // TODO: val comments = node.leadingComments
  }

  private def _maybePrintInnerComments(): Unit = {
    if (_endWithInnerRaw) printInnerComments()

    _endWithInnerRaw = true
    _indentInnerComments = true
  }

  def printInnerComments(): Unit = {
    val node = _printStack.last
    val comments: Option[Array[Comment]] =
      // node.innerComments // TODO: Get correct node
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

  def noIndentInneerCommentsHere(): Unit = _indentInnerComments = false

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

  private def _printComment(comment: Comment, skipNewLines: CommentSkipNewLine) = {
    val isBlockComment = comment.kind == CommentKind.Block
    val printNewLines =
      isBlockComment && skipNewLines != CommentSkipNewLine.All && !_noLineTerminator
    
    if (printNewLines && _buf.hasContent && skipNewLines != CommentSkipNewLine.Leading)
      newline(1)

    val value: String = if (isBlockComment) {
      if (format.adjustMultilineComment) {
        // TODO: Use correct comment type
        // @see printer.ts line 973
        ???
      }
      else s"/*${comment.value}*/"
    }
    else if (!_noLineTerminator) s"//${comment.value}"
    else s"/*${comment.value}*/"

    if (endsWith('/')) _space()

    source(LocationType.Start, comment.location)
    _append(value, isBlockComment)

    if (!isBlockComment && !_noLineTerminator) newline(1, true)
    if (printNewLines && skipNewLines != CommentSkipNewLine.Trailing)
      newline(1)
  }

  private def _printComments(
    ty: CommentType,
    comments: Array[Comment],
    node: Node,
    parent: Option[Node] = None,
    lineOffset: Int = 0
  ) =
    // TODO: Finish print comments
    // @see printer.ts line 1016
    ???
}

object Printer {
  def needsParens(
    node: Option[Node],
    parent: Option[Node],
    printStack: Option[Array[Node]]
  ): Boolean = if (parent.isEmpty) false
      else {
        // TODO: check node type
        // @ see index.ts line 114
        ???
      }

  type PrintJoinOptions = printListOptions & PrintSequenceOptions
}
