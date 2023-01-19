package mlscript.codegen.generator

import scala.util.matching.Regex
import scala.collection.mutable.{ArrayBuffer, HashSet}
import mlscript.codegen.{Position, Location, LocationType}
import mlscript.codegen.ast._
import mlscript.codegen.generator.Parentheses

case class PrintSequenceOptions(
  val statement: Option[Boolean] = None,
  val indent: Option[Boolean] = None,
  val trailingCommentsLineOffset: Option[Int] = None,
  val nextNodeStartLine: Option[Int] = None,
  val separator: Option[(Printer) => Unit] = None,
  val iterator: Option[(Node, Int) => Unit] = None
)

case class PrinterOptions(
  val inForStatementInitCounter: Int = 0,
  val printStack: Array[Node] = Array()
) {
  def derive(inFor: Int = inForStatementInitCounter, ps: Array[Node] = printStack): PrinterOptions
    = PrinterOptions(inFor, ps)
}

abstract class Printer(format: Format, map: SourceMapBuilder) {
  private val PURE_ANNOTATION_RE = "^\\s*[@#]__PURE__\\s*$".r
  private val ZERO_DECIMAL_INTEGER = "\\.0+$".r
  private val NON_DECIMAL_LITERAL = "^0[box]".r
  private val HAS_NEWLINE = "[\n\r\u2028\u2029]".r
  private val HAS_BlOCK_COMMENT_END = "\\*\\/".r
  private val SCIENTIFIC_NOTATION = "e\\d".r

  private val buf = new Buffer(Some(map), this)
  private val indentString = format.indent
  private val printedComments = new HashSet[Comment]()
  private var indentLevel: Int = 0
  
  protected var _noLineTerminator = false
  private var _endsWithWord = false
  private var _endsWithInteger = false
  protected  var _endWithInnerRaw = false
  private var _indentInnerComments = true
  private var _parenPushNewlineState: Option[Boolean] = None
  private var _lastCommentLine = 0

  def print(node: Node, parent: Option[Node])(implicit options: PrinterOptions): Unit

  def generate(ast: Node): BufferOutput = {
    print(Some(ast))(PrinterOptions())
    buf.get()
  }

  def indent(): Unit =
    if (!format.compact && !format.concise) indentLevel += 1

  def dedent(): Unit =
    if (!format.compact && !format.concise) indentLevel -= 1

  def semicolon(force: Boolean = false)(implicit options: PrinterOptions): Unit = {
    if (force) appendChar(';')
    else queue(';')

    _noLineTerminator = false
  }

  def rightBrace()(implicit options: PrinterOptions): Unit = {
    if (format.minified) buf.removeLastSemicolon()
    token("}")
  }

  def space(force: Boolean = false)(implicit options: PrinterOptions): Unit =
    if (!format.compact) {
      if (force) queue(' ')
      else if (buf.hasContent) {
        val lastCp = getLastChar()
        if (lastCp != ' ' && lastCp != '\n')
          queue(' ')
      }
    }

  def word(str: String, noLineTerminatorAfter: Boolean = false)(implicit options: PrinterOptions): Unit = {
    _maybePrintInnerComments()
    // prevent concatenating words and creating // comment out of division and regex
    if (_endsWithWord || (str.charAt(0) == '/' && endsWith('/')))
      space()

    _append(str, false)

    _endsWithWord = true
    _noLineTerminator = noLineTerminatorAfter
  }

  def number(str: String)(implicit options: PrinterOptions): Unit = {
    word(str)

    _endsWithInteger =
      (try { java.lang.Double.parseDouble(str); true } catch {case _ => false}) &&
      (NON_DECIMAL_LITERAL findFirstIn str).isEmpty &&
      (SCIENTIFIC_NOTATION findFirstIn str).isEmpty &&
      (ZERO_DECIMAL_INTEGER findFirstIn str).isEmpty &&
      str.last != '.'
  }

  def token(str: String, maybeNewline: Boolean = false)(implicit options: PrinterOptions): Unit = {
    _maybePrintInnerComments()

    val lastChar = getLastChar()
    val strFirst = str.charAt(0)
    
    // space is mandatory to avoid outputting <!--
    // http://javascript.spec.whatwg.org/#comment-syntax
    if ((lastChar == '!' && str == "--") ||
        // Need spaces for operators of the same kind to avoid: `a+++b`
        (strFirst == '+' && lastChar == '+') ||
        (strFirst == '-' && lastChar == '-') ||
        // Needs spaces to avoid changing '34' to '34.', which would still be a valid number.
        (strFirst == '.' && _endsWithInteger))
      queue(' ')

    _append(str, maybeNewline)
    _noLineTerminator = false
  }

  def tokenChar(char: Char)(implicit options: PrinterOptions): Unit = {
    _maybePrintInnerComments()
    val lastChar = getLastChar()

    if ((char == '+' && lastChar == '+') ||
        (char == '-' && lastChar == '-') ||
        (char == '.' && lastChar == '.'))
      queue(' ')

    appendChar(char)
    _noLineTerminator = false
  }

  def newline(i: Int = 1, force: Boolean = false)(implicit options: PrinterOptions): Unit =
    if (i > 0) {
      if (!force && !format.retainLines && !format.compact && format.concise)
        space()
      else if (force) {
        for (ii <- 0 until (if (i > 2) 2 else i) - buf.getNewlineCount)
          queue('\n')
      }
    }

  def endsWith(char: Char): Boolean =
    getLastChar() == char

  def getLastChar(): Char = buf.getLastChar

  def endsWithCharAndNewline: Option[Char] =
    buf.endsWithCharAndNewline

  def removeTrailingNewline(): Unit = 
    buf.removeTrailingNewline()

  def exactSource(loc: Option[Location], node: Node, parent: Option[Node])(implicit options: PrinterOptions): Unit = {
    if (loc.isEmpty) print(node, parent)
    else {
      _catchUp(LocationType.Start, loc)
      buf.exactSource(loc, node, parent, this)
    }
  }

  def source(prop: LocationType, loc: Option[Location])(implicit options: PrinterOptions): Unit =
    if (!loc.isEmpty) {
      _catchUp(prop, loc)
      buf.source(prop, loc)
    }

  def sourceWithOffset(
    prop: LocationType, 
    loc: Option[Location],
    lineOffset: Int,
    columnOffset: Int
  )(implicit options: PrinterOptions): Unit = if (!loc.isEmpty) {
    _catchUp(prop, loc)
    buf.sourceWithOffset(prop, loc, lineOffset, columnOffset)
  }

  def withSource(
    prop: LocationType, 
    loc: Option[Location],
    node: Node,
    parent: Node
  )(implicit options: PrinterOptions): Unit = if (loc.isEmpty) print(node, Some(parent))
      else {
        _catchUp(prop, loc)
        buf.withSource(prop, loc, node, parent, this)
      }

  private def _append(str: String, maybeNewline: Boolean)(implicit options: PrinterOptions): Unit = {
    _maybeAddParen(str)
    _maybeIndent(str.charAt(0))

    buf.append(str, maybeNewline)

    _endsWithWord = false
    _endsWithInteger = false
  }

  private def appendChar(char: Char)(implicit options: PrinterOptions): Unit = {
    _maybeAddParenChar(char)
    _maybeIndent(char)

    buf.appendChar(char)

    _endsWithWord = false
    _endsWithInteger = false
  }

  private def queue(char: Char)(implicit options: PrinterOptions): Unit = {
    _maybeAddParenChar(char)
    _maybeIndent(char)

    buf.queue(char)

    _endsWithWord = false
    _endsWithInteger = false
  }

  private def _maybeIndent(firstChar: Char): Unit =
    if (shouldIndent(firstChar))
      buf.queueIndentation(indentString.charAt(0), indentLevel * indentString.length())

  private def shouldIndent(firstChar: Char) =
    indentLevel > 0 && firstChar != '\n' && endsWith('\n')

  private def _maybeAddParenChar(char: Char)(implicit options: PrinterOptions): Unit =
    if (_parenPushNewlineState.isDefined && char != ' ') {
      if (char != '\n')
        _parenPushNewlineState = None
      else {
        token("(")
        indent()
        _parenPushNewlineState = Some(true)
      }
    }

  private def _maybeAddParen(str: String)(implicit options: PrinterOptions): Unit =
    if (_parenPushNewlineState.isDefined) {
      val len = str.length()
      val noSpaceIndex = str.indexWhere((c) => c != ' ')
      if (noSpaceIndex != -1) {
        val char = str.charAt(noSpaceIndex)
        if (char != '\n') {
          if (char != '/' || noSpaceIndex + 1 == len)
            _parenPushNewlineState = None
          else {
            val charPost = str.charAt(noSpaceIndex + 1)
            if (charPost == '*') {
              (PURE_ANNOTATION_RE findFirstIn str.slice(noSpaceIndex + 2, len - 2)) match {
                case None => {
                  token("(")
                  indent()
                  _parenPushNewlineState = Some(true)
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
              _parenPushNewlineState = Some(true)
            }
          }
        }
        else {
          token("(")
          indent()
          _parenPushNewlineState = Some(true)
        }
      }
    }

  def catchUp(line: Int)(implicit options: PrinterOptions): Unit =
    if (format.retainLines && line > buf.getCurrentLine) {
      queue('\n')
      catchUp(line)
    }

  private def _catchUp(prop: LocationType, optLoc: Option[Location])(implicit options: PrinterOptions): Unit =
    if (format.retainLines)
      optLoc match
        case Some(loc) => 
          val pos = loc(prop)
          for (count <- 0 until (pos.get.line - buf.getCurrentLine)) {
            queue('\n')
          }
        case None => ()

  def printTerminatorless(node: Node, parent: Node, isLabel: Boolean)(implicit options: PrinterOptions): Unit =
    if (isLabel) {
      _noLineTerminator = true
      print(Some(node), Some(parent))
    }
    else {
      _parenPushNewlineState = Some(false)
      print(Some(node), Some(parent))

      if (_parenPushNewlineState.getOrElse(false)) {
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
  )(implicit options: PrinterOptions): Unit = node match
    case None => ()
    case Some(node) =>
      _endWithInnerRaw = false
      val oldConcise = format.concise
      if (node.compact) format.concise = true

      val printStack = options.printStack :+ node
      val shouldPrintParens: Boolean =
        if (forceParens) true
        else if (format.retainFunctionParens)
          node match {
            case exp @ FunctionExpression(_, _, _, _, _)
              if (!exp.extra.isEmpty && exp.extra.get.contains("parenthesized")) => true 
            case _ => false
          }
        else Parentheses.needsParens(node, parent, printStack.reverse.toList)

      if (shouldPrintParens) {
        token("(")
        _endWithInnerRaw = false
      }

      _lastCommentLine = 0
      _printLeadingComments(node, parent)

      val loc: Option[Location] = node match {
        case program: Program => None
        case file: File => None
        case node => node.location
      }

      exactSource(loc, node, parent)(options.derive(ps = printStack))

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

      format.concise = oldConcise
      _endWithInnerRaw = false

  // TODO: Exact node type
  def getPossibleRaw(node: Node): Option[String] =
    node.extra match
      case Some(extra) => extra.get("raw") match
        case Some(raw: String) => Some(raw) // TODO: node.value === extra.rawValue
        case _ => None
      case None => None

  def printJoin[T <: Node](nodes: Option[List[T]], parent: Node, opts: PrintSequenceOptions)(implicit options: PrinterOptions): Unit =
    nodes match {
      case Some(nodes) if (nodes.length > 0) => {
        if (!opts.indent.isEmpty) indent()

        val separator = if (opts.separator.isEmpty) None else () => opts.separator.get(this)

        nodes.zipWithIndex.foreach((node, i) => {
          // TODO: check is node is empty

          if (opts.statement.getOrElse(false))
            _printNewline(i == 0, 0)

          print(Some(node), Some(parent), false, opts.trailingCommentsLineOffset.getOrElse(0))

          opts.iterator match {
            case Some(it) => it(node, i)
            case _ => ()
          }

          opts.separator match {
            case Some(sep) if (i < nodes.length - 1) => sep(this)
            case _ => ()
          }

          if (opts.statement.getOrElse(false)) {
            if (i + 1 == nodes.length) newline(1)
            else {
              val nextNode = nodes(i + 1)
              val newlinesOpts = nextNode.location match {
                case Some(loc) => loc.start.get.line
                case _ => 0
              }

              _printNewline(true, newlinesOpts)
            }
          }
        })

        if (!opts.indent.isEmpty) dedent()
      }
      case _ => ()
    }

  def printAndIndentOnComments(node: Node, parent: Node)(implicit options: PrinterOptions) = {
    val needIndent: Boolean =
      !node.leadingComments.isEmpty && node.leadingComments.get.length > 0
    if (needIndent) indent()
    print(Some(node), Some(parent))
    if (needIndent) dedent()
  }

  def printBlock(body: Node, parent: Node)(implicit options: PrinterOptions): Unit =
    parent match {
      case _: EmptyStatement => { space(); print(Some(body), Some(parent)) }
      case _ => print(Some(body), Some(parent))
    }

  private def _printTrailingComments(node: Node, parent: Option[Node], lineOffset: Int = 0)(implicit options: PrinterOptions) = {
    val innerComments = node.innerComments
    if (!innerComments.isEmpty && innerComments.get.length > 0)
      _printComments(CommentType.Trailing, innerComments.get, node, parent, lineOffset)

    val trailingComments = node.trailingComments
    if (!trailingComments.isEmpty && trailingComments.get.length > 0)
      _printComments(CommentType.Trailing, trailingComments.get, node, parent, lineOffset)
  }

  private def _printLeadingComments(node: Node, parent: Option[Node])(implicit options: PrinterOptions) = {
    val comments = node.leadingComments
    if (!comments.isEmpty && comments.get.length > 0)
      _printComments(CommentType.Leading, comments.get, node, parent)
  }

  private def _maybePrintInnerComments()(implicit options: PrinterOptions): Unit = {
    if (_endWithInnerRaw) printInnerComments()

    _endWithInnerRaw = true
    _indentInnerComments = true
  }

  def printInnerComments()(implicit options: PrinterOptions): Unit = {
    val node = options.printStack.last
    val comments = node.innerComments
    if (comments.isDefined && comments.get.length > 0) {
      val hasSpace = endsWith(' ')
      val printedCommentsCount = printedComments.size
      if (_indentInnerComments) indent()

      _printComments(CommentType.Inner, comments.get, node)
      if (hasSpace && printedCommentsCount != printedComments.size)
        space()
      if (_indentInnerComments) dedent()
    }
  }

  def noIndentInnerCommentsHere(): Unit = _indentInnerComments = false

  def printSequence[T <: Node](nodes: List[T], parent: Node, opts: PrintSequenceOptions)(implicit options: PrinterOptions): Unit = {
    val newOpts = PrintSequenceOptions(statement = Some(true), indent = opts.indent,
      trailingCommentsLineOffset = opts.trailingCommentsLineOffset,
      nextNodeStartLine = opts.nextNodeStartLine)
    printJoin(Some(nodes), parent, newOpts)
  }

  def printList(items: List[Node], parent: Node, opts: PrintSequenceOptions)(implicit options: PrinterOptions): Unit = {
    val newOpts = PrintSequenceOptions(separator = Some(opts.separator.getOrElse(Printer.commaSeparator)),
      iterator = opts.iterator, statement = opts.statement, indent = opts.indent)
    printJoin(Some(items), parent, newOpts)
  }

  private def _printNewline(newline: Boolean, nextNodeStartLine: Int)(implicit options: PrinterOptions) = 
    if (!format.retainLines && !format.compact) {
      if (format.concise) space()
      else if (newline) {
        val startLine = nextNodeStartLine
        val lastCommentLine = _lastCommentLine
        val offset = startLine - lastCommentLine

        if (startLine > 0 && lastCommentLine > 0 && offset >= 0)
          this.newline(if (offset > 0) offset else 1)
        else if (buf.hasContent) this.newline(1)
      }
    }

  private def _shouldPrintComment(comment: Comment): PrintCommentHint =
    if (comment.ignore.getOrElse(false)) PrintCommentHint.Skip
    else if (printedComments.contains(comment)) PrintCommentHint.Skip
    else if (_noLineTerminator &&
      (!(HAS_NEWLINE findFirstIn comment.value).isEmpty || !(HAS_BlOCK_COMMENT_END findFirstIn comment.value).isEmpty))
      PrintCommentHint.Defer
    else {
      printedComments.add(comment)
      if (!format.shouldPrintComment(comment.value)) PrintCommentHint.Skip
      else PrintCommentHint.Allow
    }

  private def _printComment(comment: Comment, skipNewLines: CommentSkipNewLine)(implicit options: PrinterOptions) = {
    val isBlockComment = comment.kind == CommentKind.Block
    val printNewLines =
      isBlockComment && skipNewLines != CommentSkipNewLine.All && !_noLineTerminator
    
    if (printNewLines && buf.hasContent && skipNewLines != CommentSkipNewLine.Leading)
      newline(1)

    val value: String = if (isBlockComment) {
      if (format.adjustMultilineComment) {
        val offset = comment.location match {
          case Some(location) => location.start.get.column
          case _ => 0
        }

        val newlineVal =
          if (offset > 0) (s"/*${comment.value}*/").replaceAll(s"\\n\\s{1,$offset}", "\n")
          else s"/*${comment.value}*/"
        val indentSize =
          (if (format.retainLines) 0 else buf.getCurrentColumn) +
          (if (shouldIndent('/') || format.retainLines) indentLevel * indentString.length() else 0)

        newlineVal.replaceAll("\n(?!$)", s"\n${" " * indentSize}")
      }
      else s"/*${comment.value}*/"
    }
    else if (!_noLineTerminator) s"//${comment.value}"
    else s"/*${comment.value}*/"

    if (endsWith('/')) queue(' ')

    source(LocationType.Start, comment.location)
    _append(value, isBlockComment)

    if (!isBlockComment && !_noLineTerminator) newline(1, true)
    if (printNewLines && skipNewLines != CommentSkipNewLine.Trailing)
      newline(1)
  }

  private def _printComments(
    ty: CommentType,
    comments: List[Comment],
    node: Node,
    parent: Option[Node] = None,
    lineOffset: Int = 0
  )(implicit options: PrinterOptions) = {
    val nodeLoc = node.location
    val len = comments.length
    var hasLoc = !nodeLoc.isEmpty
    val nodeStartLine = if (hasLoc) nodeLoc.get.start.get.line else 0
    val nodeEndLine = if (hasLoc) nodeLoc.get.end.get.line else 0
    var lastLine = 0
    var leadingCommentNewline = 0
    var defered = false

    val maybeNewline =
      if (_noLineTerminator)
        (offset: Int) => {}
      else (offset: Int) => newline(offset)

    comments.zipWithIndex.foreach((comment, i) => if (!defered) {
      val shouldPrint = _shouldPrintComment(comment)
      if (shouldPrint == PrintCommentHint.Defer) {
        defered = true
        hasLoc = false
      }

      if (hasLoc && !comment.location.isEmpty && shouldPrint == PrintCommentHint.Allow) {
        val commentStartLine = comment.location.get.start.get.line
        val commentEndLine = comment.location.get.end.get.line
        if (ty == CommentType.Leading) {
          val offset =
            if (i == 0) {
              if (buf.hasContent && (comment.kind == CommentKind.Line || commentStartLine != commentEndLine)) {
                leadingCommentNewline = 1
                1
              }
              else 0
            }
            else commentStartLine - lastLine

          lastLine = commentEndLine
          maybeNewline(offset)
          _printComment(comment, CommentSkipNewLine.All)

          if (i + 1 == len) {
            maybeNewline(scala.math.max(nodeStartLine - lastLine, leadingCommentNewline))
            lastLine = nodeStartLine
          }
        }
        else if (ty == CommentType.Inner) {
          val offset = commentStartLine -
            (if (i == 0) nodeStartLine else lastLine)
          lastLine = commentEndLine

          maybeNewline(offset)
          _printComment(comment, CommentSkipNewLine.All)
          if (i + 1 == len) {
            maybeNewline(scala.math.min(1, nodeEndLine - lastLine))
            lastLine = nodeEndLine
          }
        }
        else {
          val offset = commentStartLine -
            (if (i == 0) nodeEndLine - lineOffset else lastLine)
          lastLine = commentEndLine

          maybeNewline(offset)
          _printComment(comment, CommentSkipNewLine.All)
        }
      }
      else {
        hasLoc = false
        if (shouldPrint == PrintCommentHint.Allow) {
          if (len == 1) {
            val singleLine =
            if (!comment.location.isEmpty)
              (comment.location.get.start.get.line == comment.location.get.end.get.line)
            else (HAS_NEWLINE findFirstIn comment.value).isEmpty

            val shouldSkipNewline = singleLine // TODO: Check node type
            if (ty == CommentType.Leading)
              _printComment(comment, // TODO: Check node type
                if (shouldSkipNewline || singleLine) CommentSkipNewLine.All
                else CommentSkipNewLine.Default)
            else if (shouldSkipNewline && ty == CommentType.Trailing)
              _printComment(comment, CommentSkipNewLine.All)
            else _printComment(comment, CommentSkipNewLine.Default)
          }
          else if (ty == CommentType.Inner) // TODO: Check node type
            _printComment(comment,
              if (i == 0) CommentSkipNewLine.Leading
              else if (i == len - 1) CommentSkipNewLine.Trailing
              else CommentSkipNewLine.Default)
          else _printComment(comment, CommentSkipNewLine.Default)
        }
      }
    })

    if (ty == CommentType.Trailing && hasLoc && lastLine > 0)
      _lastCommentLine = lastLine
  }
}

object Printer {
  def commaSeparator(printer: Printer)(implicit options: PrinterOptions): Unit = {
    printer.token(",")
    printer.space()
  }
}
