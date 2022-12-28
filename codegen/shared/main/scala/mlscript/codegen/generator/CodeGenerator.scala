package mlscript.codegen.generator

import mlscript.codegen.ast._
import mlscript.codegen.LocationType

class CodeGenerator(
  ast: Node,
  format: Format,
  sourceMap: SourceMapBuilder
) extends Printer(format, sourceMap) {
  override def print(node: Node) = node match {
    case ThisExpression() => word("this")
    case Super() => word("super")
    case Import() => word("import")
    case EmptyStatement() => semicolon(true)
    case AnyTypeAnnotation() => word("any")
    case BooleanTypeAnnotation() => word("boolean")
    case NullLiteralTypeAnnotation() => word("null")
    case ExistsTypeAnnotation() => token("*")
    case MixedTypeAnnotation() => word("mixed")
    case EmptyTypeAnnotation() => word("empty")
    case NumberTypeAnnotation() => word("number")
    case StringTypeAnnotation() => word("string")
    case ThisTypeAnnotation() => word("this")
    case SymbolTypeAnnotation() => word("symbol")
    case VoidTypeAnnotation() => word("void")
    case JSXOpeningFragment() => { token("<"); token(">") }
    case JSXClosingFragment() => { token("</"); token(">") }
    case DebuggerStatement() => { word("debugger"); semicolon() }
    case PipelinePrimaryTopicReference() => token("#")
    case TSAnyKeyword() => word("any")
    case TSBigIntKeyword() => word("bigint")
    case TSUnknownKeyword() => word("unknown")
    case TSNumberKeyword() => word("number")
    case TSObjectKeyword() => word("object")
    case TSBooleanKeyword() => word("boolean")
    case TSStringKeyword() => word("string")
    case TSSymbolKeyword() => word("symbol")
    case TSVoidKeyword() => word("void")
    case TSUndefinedKeyword() => word("undefined")
    case TSNullKeyword() => word("null")
    case TSNeverKeyword() => word("never")
    case TSIntrinsicKeyword() => word("intrinsic")
    case TSThisType() => word("this")
    case InterpreterDirective(value) => { token(s"#!${value}"); newline(1, true) }
    case BooleanLiteralTypeAnnotation(value) =>
      if (value) word("true")
      else word("false")
    case InferredPredicate() => { token("%"); word("checks") }
    case Variance(kind) => kind match {
      case VarianceKind.Contravariant => token("+")
      case VarianceKind.Covariant => token("-")
    }
    case Placeholder(expected, name) => {
      token("%%")
      // print(Some(node.name), Some(node))
      token("%%")

      if (expected == PlaceholderExpectedNode.Statement) semicolon()
    }
    case File(program, _, _) => {
      print(program.interpreter, Some(node))
      print(Some(program), Some(node))
    }
    case Program(body, directives, _, _, _) => {
      noIndentInneerCommentsHere()
      printInnerComments()
      if (directives.length > 0) {
        val newlines = if (body.length > 0) 2 else 1
        // printSequence(directives, node, PrintSequenceOptions(
        //   None, None, newlines, None, None
        // ))

        directives.last.trailingComments match {
          case Some(comments) if (comments.length > 0) => newline(newlines)
          case _ => ()
        }
      }

      // printSequence(body, node)
    }
    case BlockStatement(body, directives) => {
      token("{")
      if (directives.length > 0) {
        val newlines = if (body.length > 0) 2 else 1
        // printSequence(directives, node, PrintSequenceOptions(
        //   None, Some(true), newlines, None, None
        // ))

        directives.last.trailingComments match {
          case Some(comments) if (comments.length > 0) => newline(newlines)
          case _ => ()
        }
      }

      // printSequence(body, node, PrintSequenceOptions(
      //     None, Some(true), None, None, None
      // ))

      sourceWithOffset(LocationType.End, node.location, 0, -1)
      rightBrace
    }
    case Directive(value) => {
      // print(Some(value), Some(node))
      semicolon()
    }
    case DirectiveLiteral(value) => {
      val raw = getPossibleRaw(node)
      if (!format.minified && raw.isDefined)
        token(raw.get)
      else {
        val unescapedSingleQuoteRE = "(?:^|[^\\])(?:\\\\)*'".r
        val unescapedDoubleQuoteRE = "(?:^|[^\\])(?:\\\\)*\"".r

        if ((unescapedDoubleQuoteRE findFirstIn value).isEmpty)
          token(s"\"$value\"")
        else if ((unescapedSingleQuoteRE findFirstIn value).isEmpty)
          token(s"'$value'")
        else
          throw new Exception("Malformed AST: it is not possible to print a directive containing both unescaped single and double quotes.")
      }
    }
    case UnaryExpression(operator, argument, _) => {
      operator match {
        case UnaryOperator.Void => word("void"); space()
        case UnaryOperator.Delete => word("delete"); space()
        case UnaryOperator.TypeOf => word("typeof"); space()
        // TODO: throw?
        case UnaryOperator.BitwiseNot => token("~")
        case UnaryOperator.LogicalNot => token("!")
        case UnaryOperator.Negation => token("-")
        case UnaryOperator.Plus => token("+")
      }

      // print(Some(argument), Some(node))
    }
    case DoExpression(body, async) => {
      if (async) {
        word("async", true)
        space()
      }

      word("do")
      space()
      // print(Some(body), Some(node))
    }
    case ParenthesizedExpression(exp) => {
      token("(")
      // print(Some(exp), Some(node))
      token(")")
    }
    case UpdateExpression(op, arg, prefix) =>
      if (prefix) {
        // TODO op2string token(op)
        // print(Some(arg), Some(node))
      }
      else {
        // printTerminatorless(arg, node, true)
        // TODO op2string token(op)
      }
    case ConditionalExpression(test, cons, alt) => {
      // print(Some(test), Some(node))
      space(); token("?"); space()
      // print(Some(cons), Some(node))
      space(); token(":"); space()
      // print(Some(alt), Some(node))
    }
    case ne @ NewExpression(callee, args) => {
      word("new"); space()
      print(Some(callee), Some(node))
      
    }
    case _ => () // TODO
  }

  def generate() = super.generate(ast)
}

object CodeGenerator {
  def apply(ast: Node, format: Format, sourceMap: SourceMapBuilder) =
    new CodeGenerator(ast, format, sourceMap)
}
