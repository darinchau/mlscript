package mlscript.codegen.generator

import mlscript.codegen.ast._

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
    case JSXOpeningElement(_, _, _) => {token("<"); token(">")}
    case JSXClosingElement(_) => {token("</"); token(">")}
    case DebuggerStatement() => {word("debugger"); semicolon()}
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
    case _ => () // TODO
  }

  def generate() = super.generate(ast)
}

object CodeGenerator {
  def apply(ast: Node, format: Format, sourceMap: SourceMapBuilder) =
    new CodeGenerator(ast, format, sourceMap)
}
