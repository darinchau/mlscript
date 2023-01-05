package mlscript.codegen.generator

import mlscript.codegen.LocationType
import mlscript.codegen.ast._
import mlscript.codegen.LocationType

class CodeGenerator(
  ast: Node,
  format: Format,
  sourceMap: SourceMapBuilder
) extends Printer(format, sourceMap):

  override def print(node: Node, parent: Option[Node])(implicit inForStatementInitCounter: Int): Unit = node match
    // BEGIN classes.ts
    case node @ ClassDeclaration(id, superClass, body, decorators) =>
      printJoin(decorators, node, PrintSequenceOptions())
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
        printList(implements, node, PrintSequenceOptions())
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
          printSequence(body, node, PrintSequenceOptions())
          dedent()
          if (!endsWith('\n')) newline()
          sourceWithOffset(LocationType.End, node.location, 0, -1)
          rightBrace()
    case node @ ClassProperty(key, value, typeAnnotation, decorators, computed, static) =>
      printJoin(decorators, node, PrintSequenceOptions())
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
    case node @ ClassAccessorProperty(key, value, typeAnnotation, decorators, computed, static) =>
      printJoin(decorators, node, PrintSequenceOptions())
      node.key.location.flatMap(_.end).map(_.line).foreach(catchUp)
      // tsPrintClassMemberModifiers(node) // TODO: Not implemented yet
      word("accessor", true)
      space()
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
    case StaticBlock(body) =>
      word("static")
      space()
      token("{")
      body match
        case Nil => token("}")
        case _ =>
          newline()
          printSequence(body, node, PrintSequenceOptions(indent = Some(true)))
          sourceWithOffset(LocationType.End, node.location, 0, -1)
          rightBrace()
    // END classes.ts
    // BEGIN flow.ts
    
    // END flow.ts
    // BEGIN jsx.ts
    case JSXAttribute(name, value) =>
      print(Some(name), Some(node))
      value.foreach { _ =>
        token("=")
        print(value, Some(node))
      }
    case JSXIdentifier(name) => word(name)
    case JSXNamespacedName(namespace, name) =>
      print(Some(namespace), Some(node))
      token(":")
      print(Some(name), Some(node))
    case JSXSpreadAttribute(argument) =>
      token("{")
      token("...")
      print(Some(argument), Some(node))
      token("}")
    case JSXExpressionContainer(expression) =>
      token("{")
      print(Some(expression), Some(node))
      token("}")
    case JSXSpreadChild(expression) =>
      token("{")
      token("...")
      print(Some(expression), Some(node))
      token("}")
    case JSXText(value) =>
      getPossibleRaw(node) match
        case None => token(value, true)
        case Some(raw) => token(raw, true)
    case JSXElement(openingElement, closingElement, children, selfClosing) =>
      print(Some(openingElement), Some(node))
      if (!openingElement.selfClosing) {
        indent()
        children.foreach { child => print(Some(child), Some(node)) }
        dedent()
        print(closingElement, Some(node))
      }
    case node @ JSXOpeningElement(name, attributes, selfClosing) =>
      token("<")
      print(Some(name), Some(node))
      print(node.typeParameters, Some(node)) // TS
      if (!attributes.isEmpty) {
        space()
        printJoin(Some(attributes), node, PrintSequenceOptions(separator = Some((p: Printer)=>p.space())))
      }
      if (selfClosing) {
        space()
        token("/>")
      } else {
        token(">")
      }
    case JSXClosingElement(name) =>
      token("</")
      print(Some(name), Some(node))
      token(">")
    case JSXEmptyExpression() =>
      printInnerComments()
    case JSXFragment(openingFragment, closingFragment, children) =>
      print(Some(openingFragment), Some(node))
      indent()
      children.foreach { child => print(Some(child), Some(node)) }
      dedent()
      print(Some(closingFragment), Some(node))
    case JSXOpeningFragment() =>
      token("<")
      token(">")
    case JSXClosingFragment() =>
      token("</")
      token(">")
    // END jsx.ts

    // BEGIN flow.ts
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
    case BooleanLiteralTypeAnnotation(value) =>
      if (value) word("true")
      else word("false")
    case InferredPredicate() => { token("%"); word("checks") }
    case Variance(kind) => kind match {
      case VarianceKind.Contravariant => token("+")
      case VarianceKind.Covariant => token("-")
    }
    // To be continued...
    // END flow.ts

    // BEGIN modules.ts
    case node @ ImportSpecifier(local, imported) =>
      node.importKind match
        case Some(ImportKind.Type) =>
          word("type")
          space()
        case Some(ImportKind.TypeOf) =>
          word("typeof")
          space()
        case _ => ()
      print(Some(imported), Some(node))
      imported match
        case Identifier(name) if local.name != name =>
          space()
          word("as")
          space()
          print(Some(local), Some(node))
        case _ => ()
    case ImportDefaultSpecifier(local) =>
      print(Some(local), Some(node))
    case ExportDefaultSpecifier(exported) =>
      print(Some(exported), Some(node))
    case node @ ExportSpecifier(local, exported) =>
      node.exportKind match
        case Some(ExportKind.Type) =>
          word("type")
          space()
        case _ => ()
      print(Some(local), Some(node))
      exported match
        case Identifier(name) if local.name != name =>
          space()
          word("as")
          space()
          print(Some(local), Some(node))
        case _ => ()
      
    case ExportNamespaceSpecifier(exported) =>
      token("*")
      space()
      word("as")
      space()
      print(Some(exported), Some(node))
    // To be continued...
    // END modules.ts
    case ThisExpression() => word("this")
    case Super() => word("super")
    case Import() => word("import")
    case EmptyStatement() => semicolon(true)
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
      noIndentInnerCommentsHere()
      printInnerComments()
      if (directives.length > 0) {
        val newlines = if (body.length > 0) 2 else 1
        printSequence(directives, node, PrintSequenceOptions(
          trailingCommentsLineOffset = Some(newlines)
        ))

        directives.last.trailingComments match {
          case Some(comments) if (comments.length > 0) => newline(newlines)
          case _ => ()
        }
      }

      printSequence(body, node, PrintSequenceOptions())
    }
    case BlockStatement(body, directives) => {
      token("{")
      if (directives.length > 0) {
        val newlines = if (body.length > 0) 2 else 1
        printSequence(directives, node, PrintSequenceOptions(
          indent = Some(true), trailingCommentsLineOffset = Some(newlines)
        ))

        directives.last.trailingComments match {
          case Some(comments) if (comments.length > 0) => newline(newlines)
          case _ => ()
        }
      }

      printSequence(body, node, PrintSequenceOptions(indent = Some(true)))

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
      def run() = {
        // TODO: seems not correct here yet
        // print(Some(ne.typeArguments), Some(ne))
        // print(Some(ne.typeParameters), Some(ne))
        token("(")
        printList(args, ne, PrintSequenceOptions())
        token(")")
      }
      parent match {
        case Some(CallExpression(callee, _)) if (callee.equals(ne)) => run()
        case Some(m: MemberExpression) => run()
        case Some(ne: NewExpression) => run()
        case _ if (!format.minified || args.length > 0) => run()
        case _ => ()
      }
    }
    case SequenceExpression(exps) =>
      printList(exps, node, PrintSequenceOptions())
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
      printList(args, node, PrintSequenceOptions())
      token(")")
    }
    case ce @ CallExpression(callee, args) => {
      print(Some(callee), Some(node))
      print(ce.typeArguments, Some(node))
      print(ce.typeParameters, Some(node))
      token("(")
      printList(args, node, PrintSequenceOptions())
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
        op.equals("in") && Printer.needsParens(Some(node), parent, None)
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
    case JSXMemberExpression(obj, prop) => {
      print(Some(obj), Some(node))
      token(".")
      print(Some(prop), Some(node))
    }
    case dec @ ExportAllDeclaration(source) => {
      word("export"); space()
      dec.exportKind match {
        case Some(value) if (value == ExportKind.Type) => {
          word("type"); space()
        }
        case _ => ()
      }

      token("*"); space(); word("from"); space()
      // @ts-expect-error
      print(Some(source), Some(node))
    }
    case dec @ ExportNamedDeclaration(declaration, specifiers, source) => {
      word("export"); space()
      declaration match {
        case Some(value) => {
          print(declaration, Some(node))
          value match {
            case s: Statement => semicolon()
            case _ => ()
          }
        }
        case _ => dec.exportKind match {
          case Some(value) if (value == ExportKind.Type) => {
            word("type"); space()
          }
          case _ => ()
        }
      }

      val res = specifiers.iterator.zipWithIndex.foldLeft((false, -1))(
        (res, p) => if (!res._1 && res._2 > -1) res else p._1 match {
          case s: ExportDefaultSpecifier => {
            print(Some(s), Some(node))
            if (p._2 < specifiers.length) { token(","); space() }
            (true, p._2)
          }
          case s: ExportNamespaceSpecifier => {
            print(Some(s), Some(node))
            if (p._2 < specifiers.length) { token(","); space() }
            (true, p._2)
          }
          case _ => (res._1, p._2)
        })

      if (res._2 < specifiers.length || res._1) {
        token("{")
        if (res._2 < specifiers.length) {
          space()
          printList(specifiers.drop(res._2), node, PrintSequenceOptions())
          space()
        }
        token("}")
      }

      source match {
        case Some(value) => {
          space(); word("from"); space()
          dec.assertions match {
            case Some(assertions) if (assertions.length > 0) => {
              print(source, Some(node), true)
              space()
              word("assert"); space(); token("{"); space()
              printList(assertions, node, PrintSequenceOptions())
              space(); token("}")
            }
            case _ => print(source, Some(node))
          }
        }
        case _ => ()
      }

      semicolon()
    }
    case ExportDefaultDeclaration(declaration) => {
      word("export")
      noIndentInnerCommentsHere()
      space(); word("default"); space()
      print(Some(declaration), Some(node))
      declaration match {
        case s: Statement => semicolon()
        case _ => ()
      }
    }
    case dec @ ImportDeclaration(specifiers, source) => {
      word("import"); space()
      dec.importKind match {
        case Some(ImportKind.Type) => {
          noIndentInnerCommentsHere()
          word("type"); space()
        }
        case Some(ImportKind.TypeOf) => {
          noIndentInnerCommentsHere()
          word("typeof"); space()
        }
        case _ if (dec.module.isDefined) => {
          noIndentInnerCommentsHere()
          word("module"); space()
        }
        case _ => ()
      }

      val res = specifiers.iterator.zipWithIndex.foldLeft((!specifiers.isEmpty, -1))(
        (res, p) => if (!res._1) res else p._1 match {
          case s: ImportDefaultSpecifier => {
            print(Some(s), Some(node))
            if (p._2 < specifiers.length) { token(","); space() }
            (true, p._2)
          }
          case s: ImportNamespaceSpecifier => {
            print(Some(s), Some(node))
            if (p._2 < specifiers.length) { token(","); space() }
            (true, p._2)
          }
          case _ => (false, p._2)
        })

      if (res._2 < specifiers.length) {
        token("{")
        space()
        printList(specifiers.drop(res._2), node, PrintSequenceOptions())
        space()
        token("}")
      }
      else if (!specifiers.isEmpty) dec.importKind match {
        case Some(ImportKind.Type) => {
          space(); word("from"); space()
        }
        case Some(ImportKind.TypeOf) => {
          space(); word("from"); space()
        }
        case _ => ()
      }

      dec.assertions match {
        case Some(value) => {
          print(Some(source), Some(node), true)
          space()
          word("assert"); space(); token("{"); space()
          printList(value, node, PrintSequenceOptions())
          space(); token("}")
        }
        case None => print(Some(source), Some(node))
      }

      semicolon()
    }
    case ImportAttribute(key, value) => {
      print(Some(key))
      token(":"); space()
      print(Some(value))
    }
    case ImportNamespaceSpecifier(local) => {
      token("*"); space()
      word("as"); space()
      print(Some(local), Some(node))
    }
    case exp @ TaggedTemplateExpression(tag, quasi) => {
      print(Some(tag), Some(node))
      print(exp.typeParameters, Some(node))
      print(Some(quasi), Some(node))
    }
    case TemplateElement(value, tail) => {
      val pos = parent match {
        case Some(TemplateLiteral(quasis, _)) =>
          (quasis.head.equals(node), quasis.last.equals(node))
        case _ => throw new AssertionError("wrong parent type of TemplateElement") // This should not happen
      }

      // TODO: value.raw?
      val tok = (if (pos._1) "`" else "}") + value.toString() + (if (pos._2) "`" else "${")
      token(tok)
    }
    case TemplateLiteral(quasis, expressions) => {
      quasis.iterator.zipWithIndex.foreach((q, i) => {
        print(Some(q), Some(node))
        if (i + 1 < quasis.length) print(Some(expressions(i)), Some(node))
      })
    }
    case TSTypeAnnotation(anno) => {
      token(":"); space()
      // @ts-expect-error node.optional
      print(Some(anno), Some(node))
    }
    case TSTypeParameterInstantiation(params) => {
      token("<")
      printList(params, node, PrintSequenceOptions())
      parent match {
        case Some(ArrowFunctionExpression(params, _, _, _)) if (params.length == 1) =>
          token(",")
        case _ => () 
      }
      token(">")
    }
    case tp @ TSTypeParameter(constraint, default, name) => {
      if (tp.in.getOrElse(false)) {
        word("in"); space()
      }
      if (tp.out.getOrElse(false)) {
        word("out"); space()
      }
      
      word(name)
      
      if (constraint.isDefined) {
        space(); word("extends"); space()
        print(constraint, Some(node))
      }
      if (default.isDefined) {
        space(); word("="); space()
        print(default, Some(node))
      }
    }
    case prop @ TSParameterProperty(params) => {
      prop.accessibility match {
        case Some(AccessModifier.Private) => {
          word("private"); space()
        }
        case Some(AccessModifier.Public) => {
          word("public"); space()
        }
        case Some(AccessModifier.Protected) => {
          word("protected"); space()
        }
        case _ => ()
      }

      if (prop.readonly.getOrElse(false)) {
        word("readonly"); space()
      }

      // TODO: _param(node.parameter)
    }
    case fun @ TSDeclareFunction(id, tp, params, ret) => {
      if (fun.declare.getOrElse(false)) {
        word("declare"); space()
      }
      // TODO: _functionHead(node)
      token(";")
    }
    case method @ TSDeclareMethod(dec, key, tp, params, ret) => {
      // TODO: _classMethodHead(node)
      token(";")
    }
    case TSQualifiedName(left, right) => {
      print(Some(left), Some(node))
      token(".")
      print(Some(right), Some(node))
    }
    case TSCallSignatureDeclaration(tp, params, anno) => {
      print(tp, Some(node))
      token("(")
      // TODO: _parameters(parameters, node)
      token(")")
      // TODO: No return type found
      print(anno, Some(node))
      token(";")
    }
    case TSConstructSignatureDeclaration(tp, params, anno) => {
      word("new"); space()
      print(tp, Some(node))
      token("(")
      // TODO: _parameters(parameters, node)
      token(")")
      // TODO: No return type found
      print(anno, Some(node))
      token(";")
    }
    case sig @ TSPropertySignature(key, anno, init, kind) => {
      if (sig.readonly.getOrElse(false)) {
        word("readonly"); space()
      }
      if (sig.computed.getOrElse(false)) token("[")
      print(Some(key), Some(node))
      if (sig.computed.getOrElse(false)) token("]")
      if (sig.optional.getOrElse(false)) token("?")
      print(anno, Some(node))
      if (init.isDefined) {
        space(); token("="); space()
        print(init, Some(node))
      }
      token(";")
    }
    case sig @ TSMethodSignature(key, tp, params, anno, kind) => {
      kind match {
        case TSMethodSignatureKind.Getter => { word("get"); space() }
        case TSMethodSignatureKind.Setter => { word("set"); space() }
        case _ => ()
      }

      if (sig.computed.getOrElse(false)) token("[")
      print(Some(key), Some(node))
      if (sig.computed.getOrElse(false)) token("]")
      if (sig.optional.getOrElse(false)) token("?")

      print(tp, Some(node))
      token("(")
      // TODO: _parameters(parameters, node)
      token(")")
      // TODO: No return type found
      print(anno, Some(node))

      token(";")
    }
    case sig @ TSIndexSignature(params, anno) => {
      if (sig.static.getOrElse(false)) {
        word("static"); space()
      }
      if (sig.readonly.getOrElse(false)) {
        word("readonly"); space()
      }
      token("[")
      // TODO: _parameters(node.parameters, node)
      token("]")
      print(anno, Some(node))
      token(";")
    }
    case TSFunctionType(tp, params, anno) => {
      print(tp, Some(node))
      token("(")
      // TODO: _parameters(parameters, node)
      token(")"); space(); token("=>"); space()
      // TODO: No return type found
      print(anno, Some(node))
    }
    case ct @ TSConstructorType(tp, params, anno) => {
      if (ct.`abstract`.getOrElse(false)) {
        word("abstract"); space()
      }

      word("new"); space()

      print(tp, Some(node))
      token("(")
      // TODO: _parameters(parameters, node)
      token(")"); space(); token("=>"); space()
      // TODO: No return type found
      print(anno, Some(node))
    }
    case TSTypeReference(name, tp) => {
      print(Some(name), Some(node), true)
      print(tp, Some(node), true)
    }
    case TSTypePredicate(name, anno, asserts) => {
      if (asserts.getOrElse(false)) {
        word("asserts"); space()
      }

      print(Some(name))
      anno match {
        case Some(value) => {
          space(); word("is"); space()
          print(Some(value.typeAnnotation))
        }
        case _ => ()
      }
    }
    case TSTypeQuery(name, tp) => {
      word("typeof"); space()
      print(Some(name))
      if (tp.isDefined) print(tp, Some(node))
    }
    case TSTypeLiteral(members) => {
      tsPrintBraced(members, node)
    }
    case TSArrayType(element) => {
      print(Some(element), Some(node), true)
      token("[]")
    }
    case TSTupleType(element) => {
      token("[")
      printList(element, node, PrintSequenceOptions())
      token("]")
    }
    case TSOptionalType(anno) => {
      print(Some(anno), Some(node))
      token("?")
    }
    case TSRestType(anno) => {
      token("...")
      print(Some(anno), Some(node))
    }
    case TSNamedTupleMember(label, element, optional) => {
      print(Some(label), Some(node))
      if (optional) token("?")
      token(":")
      space()
      print(Some(element), Some(node))
    }
    case TSUnionType(types) =>
      tsPrintUnionOrIntersectionType(types, node, "|")
    case TSIntersectionType(types) =>
      tsPrintUnionOrIntersectionType(types, node, "&")
    case TSConditionalType(checkType, extendsType, trueType, falseType) => {
      print(Some(checkType))
      space(); word("extends"); space()
      print(Some(extendsType))
      space(); token("?"); space()
      print(Some(trueType))
      space(); token(":"); space()
      print(Some(falseType))
    }
    case TSInferType(tp) => {
      token("infer"); space()
      print(Some(tp))
    }
    case TSParenthesizedType(anno) => {
      token("(")
      print(Some(anno), Some(node))
      token(")")
    }
    case TSTypeOperator(anno, op) => {
      word(op); space()
      print(Some(anno), Some(node))
    }
    case TSIndexedAccessType(obj, index) => {
      print(Some(obj), Some(node), true)
      token("[")
      print(Some(index), Some(node))
      token("]")
    }
    case map @ TSMappedType(tp, anno, name) => {
      token("{"); space()
      map.readonly match {
        case Some("+") => token("+"); word("readonly"); space()
        case Some("-") => token("-"); word("readonly"); space()
        case Some(true) => word("readonly"); space()
        case _ => ()
      }

      token("[")
      name match {
        case Some(Identifier(name)) => word(name)
        case _ => ()
      }
      space(); word("in"); space()
      print(tp.constraint, Some(tp))
      if (name.isDefined) {
        space(); word("as"); space()
        print(name, Some(node))
      }
      token("]")
      map.readonly match {
        case Some("+") => token("+"); token("?")
        case Some("-") => token("-"); token("?")
        case Some(true) => token("?")
        case _ => ()
      }
      token(":"); space()
      print(anno, Some(node))
      space(); token("}")
    }
    case TSLiteralType(lit) => print(Some(lit), Some(node))
    case TSExpressionWithTypeArguments(exp, tp) => {
      print(Some(exp), Some(node))
      print(tp, Some(node))
    }
    case dec @ TSInterfaceDeclaration(id, tp, ext, body) => {
      if (dec.declare.getOrElse(false)) {
        word("declare"); space()
      }
      word("interface"); space()
      print(Some(id), Some(node))
      print(tp, Some(node))
      ext match {
        case Some(value) if (value.length > 0) => {
          space(); word("extends"); space()
          printList(value, node, PrintSequenceOptions())
        }
        case _ => ()
      }

      space()
      print(Some(body), Some(node))
    }
    case TSInterfaceBody(body) => tsPrintBraced(body, node)
    case dec @ TSTypeAliasDeclaration(id, tp, anno) => {
      if (dec.declare.getOrElse(false)) {
        word("declare"); space()
      }

      word("type"); space()
      print(Some(id), Some(node))
      print(tp, Some(node))
      space(); token("="); space()
      print(Some(anno), Some(node))
      token(";")
    }
    case TSAsExpression(exp, anno) => {
      val forceParens = exp.trailingComments match {
        case Some(value) => value.length > 0
        case _ => false
      }
      print(Some(exp), Some(node), true, 0, forceParens) // TODO: undefined?
      space(); word("as"); space()
      print(Some(anno), Some(node))
    }
    case TSSatisfiesExpression(exp, anno) => {
      val forceParens = exp.trailingComments match {
        case Some(value) => value.length > 0
        case _ => false
      }
      print(Some(exp), Some(node), true, 0, forceParens) // TODO: undefined?
      space(); word("satisfies"); space()
      print(Some(anno), Some(node))
    }
    case TSTypeAssertion(anno, exp) => {
      token("<")
      print(Some(anno), Some(node))
      token(">"); space()
      print(Some(exp), Some(node))
    }
    case TSInstantiationExpression(exp, tp) => {
      print(Some(exp), Some(node))
      print(tp, Some(node))
    }
    case dec @ TSEnumDeclaration(id, members) => {
      if (dec.declare.getOrElse(false)) {
        word("declare"); space()
      }

      if (dec.const.getOrElse(false)) {
        word("const"); space()
      }

      word("enum"); space()
      print(Some(id), Some(node))
      space()
      tsPrintBraced(members, node)
    }
    case TSEnumMember(id, init) => {
      print(Some(id), Some(node))
      if (init.isDefined) {
        space(); token("="); space()
        print(init, Some(node))
      }
      token(",")
    }
    case dec @ TSModuleDeclaration(id, body) => {
      if (dec.declare.getOrElse(false)) {
        word("declare"); space()
      }

      if (dec.global.getOrElse(false)) {
        id match {
          case i: Identifier => word("namespace")
          case _ => word("module")
        }
      }
      print(Some(id), Some(node))
      // TODO: should body be optional

      def run (md: TSModuleDeclaration): Unit = {
        token(".")
        print(Some(md.id), Some(md))
        md.body match {
          case m: TSModuleDeclaration => run(m)
          case _ => ()
        }
      }

      body match {
        case m: TSModuleDeclaration => run(m)
        case _ => ()
      }

      space()
      print(Some(body), Some(node))
    }
    case TSModuleBlock(body) => tsPrintBraced(body, node)
    case TSImportType(arg, qualifier, tp) => {
      word("import"); token("(")
      print(Some(arg), Some(node))
      token(")")
      if (qualifier.isDefined) {
        token(".")
        print(qualifier, Some(node))
      }
      if (tp.isDefined) {
        print(tp, Some(node))
      }
    }
    case dec @ TSImportEqualsDeclaration(id, ref, exp) => {
      if (exp) { word("export"); space() }
      word("import"); space()
      print(Some(id), Some(node))
      space()
      token("="); space()
      print(Some(ref), Some(node))
      token(";")
    }
    case TSExternalModuleReference(exp) => {
      token("require(")
      print(Some(exp), Some(node))
      token(")")
    }
    case TSNonNullExpression(exp) => {
      print(Some(exp), Some(node))
      token("!")
    }
    case TSExportAssignment(exp) => {
      word("export"); space(); token("="); space()
      print(Some(exp), Some(node))
      token(";")
    }
    case TSNamespaceExportDeclaration(id) => {
      word("export"); space(); token("as"); space(); word("namespace"); space()
      print(Some(id), Some(node))
    }
    case Identifier(name) => word(name)
    case _ => () // TODO

  def generate() = super.generate(ast)

  private def tsPrintBraced(members: List[Node], node: Node)(implicit inForStatementInitCounter: Int) = {
    token("{")
    if (members.length > 0) {
      indent()
      newline()
      members.foreach((m) => {
        print(Some(m), Some(node))
        newline()
      })
      dedent()
    }

    sourceWithOffset(LocationType.End, node.location, 0, -1)
    rightBrace()
  }

  private def tsPrintUnionOrIntersectionType(types: List[Node], node: Node, sep: String)(implicit inForStatementInitCounter: Int) =
    printJoin(Some(types), node, PrintSequenceOptions(
      separator = Some((p: Printer) => { p.space(); p.token(sep); p.space(); })
    ))

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
