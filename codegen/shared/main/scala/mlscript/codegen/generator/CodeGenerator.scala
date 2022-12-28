package mlscript.codegen.generator

import mlscript.codegen.LocationType
import mlscript.codegen.ast._
import mlscript.codegen.LocationType

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
      print(Some(name), Some(node))
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
        printSequence(directives, node, PrintSequenceOptions(
          None, None, Some(newlines), None, None
        ))

        directives.last.trailingComments match {
          case Some(comments) if (comments.length > 0) => newline(newlines)
          case _ => ()
        }
      }

      printSequence(body, node, PrintSequenceOptions(
        None, None, None, None, None
      ))
    }
    case BlockStatement(body, directives) => {
      token("{")
      if (directives.length > 0) {
        val newlines = if (body.length > 0) 2 else 1
        printSequence(directives, node, PrintSequenceOptions(
          None, Some(true), Some(newlines), None, None
        ))

        directives.last.trailingComments match {
          case Some(comments) if (comments.length > 0) => newline(newlines)
          case _ => ()
        }
      }

      printSequence(body, node, PrintSequenceOptions(
          None, Some(true), None, None, None
      ))

      sourceWithOffset(LocationType.End, node.location, 0, -1)
      rightBrace()
    }
    case Directive(value) => {
      print(Some(value), Some(node))
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

      print(Some(argument), Some(node))
    }
    case DoExpression(body, async) => {
      if (async) {
        word("async", true)
        space()
      }

      word("do")
      space()
      print(Some(body), Some(node))
    }
    case ParenthesizedExpression(exp) => {
      token("(")
      print(Some(exp), Some(node))
      token(")")
    }
    case UpdateExpression(op, arg, prefix) =>
      if (prefix) {
        // TODO op2string token(op)
        print(Some(arg), Some(node))
      }
      else {
        printTerminatorless(arg, node, true)
        // TODO op2string token(op)
      }
    case ConditionalExpression(test, cons, alt) => {
      print(Some(test), Some(node))
      space(); token("?"); space()
      print(Some(cons), Some(node))
      space(); token(":"); space()
      print(Some(alt), Some(node))
    }
    case ne @ NewExpression(callee, args) => {
      word("new"); space()
      print(Some(callee), Some(node))
      // if (!format.minified || args.length > 0 || ne.optional)
      // TODO: need parent

      // TODO: seems not correct here yet
      // print(Some(ne.typeArguments), Some(ne))
      // print(Some(ne.typeParameters), Some(ne))
      token("(")
      printList(args, ne, PrintListOptions(None, None, None, None))
      token(")")
    }
    case SequenceExpression(exps) =>
      printList(exps, node, PrintListOptions(None, None, None, None))
    case Decorator(exp) => {
      token("@")
      if (CodeGenerator.shouldParenthesizeDecoratorExpression(exp)) {
        token("(")
        print(Some(exp), Some(node))
        token(")")
      }
      else print(Some(exp), Some(node))
      newline()
    }
    case OptionalMemberExpression(obj, prop, com, opt) => {
      print(Some(obj), Some(node))
      if (!com.getOrElse(false) && (prop match {
        case m: MemberExpression => true
        case _ => false
      })) {
        throw new Exception("Got a MemberExpression for MemberExpression property")
      }

      val computed = com.getOrElse(false) // TODO: no value found
      if (opt) token("?.")
      if (computed) {
        token("[")
        print(Some(prop), Some(node))
        token("]")
      }
      else {
        if (!opt) token(".")
        print(Some(prop), Some(node))
      }
    }
    case n @ OptionalCallExpression(callee, args, opt) => {
      print(Some(callee), Some(node))
      print(n.typeParameters, Some(node))
      if (opt) token("?.")
      print(n.typeArguments, Some(node))
      token("(")
      printList(args, node, PrintListOptions(None, None, None, None))
      token(")")
    }
    case ce @ CallExpression(callee, args) => {
      print(Some(callee), Some(node))
      print(ce.typeArguments, Some(node))
      print(ce.typeParameters, Some(node))
      token("(")
      printList(args, node, PrintListOptions(None, None, None, None))
      token(")")
    }
    case AwaitExpression(args) => {
      word("await")
      // TODO: should args be optional?
      space()
      printTerminatorless(args, node, false)
    }
    case YieldExpression(args, delegate) => {
      word("yield", true)
      if (delegate) {
        token("*")
        args match {
          case Some(_) => {
            space(); print(args, Some(node))
          }
          case _ => ()
        }
      }
      else args match {
        case Some(value) => {
          space(); printTerminatorless(value, node, false)
        }
        case _ => ()
      }
    }
    case ExpressionStatement(exp) => {
      print(Some(exp), Some(node))
      semicolon()
    }
    case AssignmentPattern(left, right) => {
      print(Some(left), Some(node))
      // TODO: @ts-expect-error
      space(); token("="); space()
      print(Some(right), Some(node))
    }
    case AssignmentExpression(op, left, right) => {
      val parens = inForStatementInitCounter > 0 &&
        op.equals("in") // TODO: need parent && Printer.needsParens(Some(node))
      if (parens) token("(")
      print(Some(left), Some(node))
      space()
      if (op.equals("in") || op.equals("instanceof"))
        word(op)
      else token(op)
      space()
      print(Some(right), Some(node))
      token(")")
    }
    case BindExpression(obj, callee) => {
      print(Some(obj), Some(node))
      token("::")
      print(Some(callee), Some(node))
    }
    case MemberExpression(obj, prop, com, opt) => {
      print(Some(obj), Some(node))
      if (!com && (prop match {
        case m: MemberExpression => true
        case _ => false
      })) {
        throw new Exception("Got a MemberExpression for MemberExpression property")
      }

      val computed = com // TODO: no value found
      if (computed) {
        token("[")
        print(Some(prop), Some(node))
        token("]")
      }
      else {
        token(".")
        print(Some(prop), Some(node))
      }
    }
    case MetaProperty(meta, prop) => {
      print(Some(meta), Some(node))
      token(".")
      print(Some(prop), Some(node))
    }
    case PrivateName(id) => {
      token("#")
      print(Some(id), Some(node))
    }
    case V8IntrinsicIdentifier(name) => { token("%"); word(name) }
    case ModuleExpression(body) => {
      word("module", true); space(); token("{"); indent()
      if (body.body.length > 0 || body.directives.length > 0)
        newline()
      print(Some(body), Some(node))
      dedent()
      sourceWithOffset(LocationType.End, node.location, 0, -1)
      rightBrace()
    }
    case JSXAttribute(name, value) => {
      print(Some(name), Some(node))
      value match {
        case Some(_) => { token("="); print(value, Some(node)) }
        case _ => ()
      }
    }
    case JSXIdentifier(name) => word(name)
    case JSXNamespacedName(ns, name) => {
      print(Some(ns), Some(node))
      token(":")
      print(Some(name), Some(node))
    }
    case JSXMemberExpression(obj, prop) => {
      print(Some(obj), Some(node))
      token(".")
      print(Some(prop), Some(node))
    }
    case JSXSpreadAttribute(args) => {
      token("{"); token("...")
      print(Some(args), Some(node))
      token("}")
    }
    case JSXExpressionContainer(exp) => {
      token("{")
      print(Some(exp), Some(node))
      token("}")
    }
    case JSXSpreadChild(exp) => {
      token("{"); token("...")
      print(Some(exp), Some(node))
      token("}")
    }
    case JSXText(txt) => {
      val raw = getPossibleRaw(node)
      token(raw.getOrElse(txt), true)
    }
    case JSXElement(op, cls, children, selfClosing) => {
      print(Some(op), Some(node))
      if (!op.selfClosing) {
        indent()
        children.foreach((child) => print(Some(child), Some(node)))
        dedent()
        print(cls, Some(node))
      }
    }
    case ele @ JSXOpeningElement(name, attr, selfClosing) => {
      token("<")
      print(Some(name), Some(node))
      print(ele.typeParameters, Some(node))
      if (attr.length > 0) {
        space()
        // printJoin
      }
      if (selfClosing) {
        space()
        token("/>")
      }
      else token(">")
    }
    case JSXClosingElement(name) => {
      token("</")
      print(Some(name), Some(node))
      token(">")
    }
    case e: JSXEmptyExpression => printInnerComments()
    case JSXFragment(op, cls, children) => {
      print(Some(op), Some(node))
      indent()
      children.foreach((child) => print(Some(child), Some(node)))
      dedent()
      print(Some(cls), Some(node))
    }
    case _ => () // TODO

  def generate() = super.generate(ast)

end CodeGenerator

object CodeGenerator:
  def apply(ast: Node, format: Format, sourceMap: SourceMapBuilder) =
    new CodeGenerator(ast, format, sourceMap)
  def isDecoratorMemberExpression(node: Node): Boolean =
    node match {
      case i: Identifier => true
      case MemberExpression(obj, prop, computed, _) =>
        !computed && (prop match {
          case i: Identifier => true
          case _ => false
        }) && isDecoratorMemberExpression(obj)
      case _ => false
    }
  def shouldParenthesizeDecoratorExpression(node: Node): Boolean =
    node match {
      case p: ParenthesizedExpression => false
      case e: CallExpression => !isDecoratorMemberExpression(e.callee)
      case _ => !isDecoratorMemberExpression(node)
    }
