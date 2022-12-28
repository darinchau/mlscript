package mlscript.codegen.generator

import mlscript.codegen.LocationType
import mlscript.codegen.ast._

class CodeGenerator(
  ast: Node,
  format: Format,
  sourceMap: SourceMapBuilder
) extends Printer(format, sourceMap):

  override def print(node: Node): Unit = node match
    // BEGIN classes.ts
    case node @ ClassDeclaration(id, superClass, body, decorators) =>
      printJoin(decorators, node, ???)
      if (node.declare.getOrElse(false)) {
        word("declare")
        space()
      }
      if (node.`abstract`.getOrElse(false)) {
        word("abstract")
        space()
      }
      word("class")
      space()
      print(Some(id), Some(node))
      print(node.typeParameters, Some(node))
      superClass.foreach { superClassExpr =>
        space()
        word("extends")
        space()
        print(superClass, Some(node))
        print(node.superTypeParameters, Some(node))
      }
      node.implements.foreach { implements =>
        space()
        word("implements")
        space()
        printList(implements, node, ???)
      }
      space()
      print(Some(node.body), Some(node))
    case node @ ClassExpression(id, superClass, body, decorators) =>
      // TODO: Extract common fields from `ClassDeclaration` and `ClassExpression`
      // to `Class` and handle them 
      ???
    case node @ ClassBody(body) =>
      token("{")
      body match
        case Nil => token("}")
        case _ =>
          newline()
          indent()
          printSequence(body, node, ???)
          dedent()
          if (!endsWith('\n')) newline()
          sourceWithOffset(LocationType.End, node.location, 0, -1)
          rightBrace()
    case node @ ClassProperty(key, value, typeAnnotation, decorators, computed, static) =>
      printJoin(decorators, node, ???)
      node.key.location.flatMap(_.end.map(_.line)).foreach(catchUp)
      // tsPrintClassMemberModifiers(node) // TODO: Not implemented yet
      if (node.computed) {
        token("[")
        print(Some(key), Some(node))
        token("]")
      } else {
        // _variance(node) // TODO: Not implemented yet
        print(Some(key), Some(node))
      }
      if (node.optional.getOrElse(false)) token("?")
      if (node.definite.getOrElse(false)) token("!")
      print(typeAnnotation, Some(node))
      value.foreach { _ =>
        space()
        token("=")
        space()
        print(value, Some(node))
      }
      semicolon()
    // END classes.ts
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

  def generate() = super.generate(ast)

end CodeGenerator

object CodeGenerator:
  def apply(ast: Node, format: Format, sourceMap: SourceMapBuilder) =
    new CodeGenerator(ast, format, sourceMap)
