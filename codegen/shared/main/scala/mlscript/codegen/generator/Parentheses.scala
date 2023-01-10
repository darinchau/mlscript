package mlscript.codegen.generator

import mlscript.codegen.ast._

object Parentheses {
  private val precedence = Map[String, Int](
    "||"-> 0, "??"-> 0, "|>"-> 0,
    "&&"-> 1,
    "|"-> 2,
    "^"-> 3,
    "&"-> 4,
    "=="-> 5, "==="-> 5, "!="-> 5, "!=="-> 5,
    "<"-> 6, ">"-> 6, "<="-> 6, ">="-> 6, "in"-> 6, "instanceof"-> 6,
    ">>"-> 7, "<<"-> 7, ">>>"-> 7,
    "+"-> 8, "-"-> 8,
    "*"-> 9, "/"-> 9, "%"-> 9,
    "**"-> 10,
  )

  private def isOrHasCallExpression(node: Node): Boolean =
    node match {
      case ce: CallExpression => true
      case MemberExpression(obj, _, _, _) => isOrHasCallExpression(obj)
      case _ => false
    }

  def needParens(node: Node, parent: Option[Node], stack: Array[Node]): Boolean =
    parent match {
      case None => false
      case Some(NewExpression(callee, _)) if (callee == node && isOrHasCallExpression(node)) => true
      case _ => handleParens(node, parent, stack)
    }

  private def hasPostfixPart(node: Node, parent: Option[Node]) =
    parent match {
      case Some(MemberExpression(obj, _, _, _)) if (obj == node) => true
      case Some(OptionalMemberExpression(obj, _, _, _)) if (obj == node) => true
      case Some(CallExpression(callee, _)) if (callee == node) => true
      case Some(OptionalCallExpression(callee, _, _)) if (callee == node) => true
      case Some(NewExpression(callee, _)) if (callee == node) => true
      case Some(TaggedTemplateExpression(tag, _)) if (tag == node) => true
      case Some(v: TSNonNullExpression) => true
      case _ => false
    }

  private def isFirstInContext(stack: Array[Node]): Boolean = false // TODO: CheckParam

  private def handleParens(node: Node, parent: Option[Node], stack: Array[Node]): Boolean =
    node match {
      case ta: NullableTypeAnnotation => parent match {
        case Some(v: ArrayTypeAnnotation) => true
        case _ => false
      }
      case ta: FunctionTypeAnnotation =>
        if (stack.length < 3) false
        else parent match {
          case Some(v: UnionTypeAnnotation) => true // (() => A) | (() => B)
          case Some(v: IntersectionTypeAnnotation) => true // (() => A) & (() => B)
          case Some(v: ArrayTypeAnnotation) => true // (() => A)[]
          case Some(v: TypeAnnotation) => stack(stack.length - 3) match { // <T>(A: T): (T => T[]) => B => [A, B]
            case af: ArrowFunctionExpression => true
            case _ => false
          }
          case _ => false
        }
      case ue: UpdateExpression => hasPostfixPart(node, parent) // TODO: isClassExtendsClause
      case oe: ObjectExpression => isFirstInContext(stack)
      case DoExpression(_, async) if (!async) => isFirstInContext(stack)
      case BinaryExpression(operator, left, right) => false // TODO:
      case ta: UnionTypeAnnotation =>
        parent match {
          case Some(ta: ArrayTypeAnnotation) => true
          case Some(ta: NullableTypeAnnotation) => true
          case Some(ta: IntersectionTypeAnnotation) => true
          case Some(ta: UnionTypeAnnotation) => true
          case _ => false
        }
      case tp: OptionalIndexedAccessType => false // TODO:
      case as: TSAsExpression => true
      case ut: TSUnionType =>
        parent match {
          case Some(tp: TSArrayType) => true
          case Some(tp: TSOptionalType) => true
          case Some(tp: TSIntersectionType) => true
          case Some(tp: TSUnionType) => true
          case Some(tp: TSRestType) => true
          case _ => false
        }
      case it: TSInferType =>
        parent match {
          case Some(tp: TSArrayType) => true
          case Some(tp: TSOptionalType) => true
          case _ => false
        }
      case _: TSInstantiationExpression =>
        parent match {
          case Some(e: CallExpression) if (e.typeParameters.isDefined) => true
          case Some(e: OptionalCallExpression) if (e.typeParameters.isDefined) => true
          case Some(e: NewExpression) if (e.typeParameters.isDefined) => true
          case Some(e: TSInstantiationExpression) if (e.typeParameters.isDefined) => true
          case _ => false
        }
      case _: SequenceExpression =>
        parent match {
          case Some(_: ForStatement) => true
          case Some(_: ThrowStatement) => true
          case Some(_: ReturnStatement) => true
          case Some(IfStatement(test, _, _)) if (test == node) => true
          case Some(WhileStatement(test, _)) if (test == node) => true
          case Some(ForInStatement(_, right, _)) if (right == node) => true
          case Some(SwitchStatement(dis, _)) if (dis == node) => true
          case Some(ExpressionStatement(exp)) if (exp == node) => true
          case _ => false
        }
      case _: YieldExpression => false // TODO: isBinary
      case _: ClassExpression => isFirstInContext(stack)
      case _: UnaryExpression => false // TODO: isClassExtendsClause
      case _: FunctionExpression => isFirstInContext(stack)
      case _: ArrowFunctionExpression => false // TODO:
      case _: ConditionalExpression => false // TODO:
      case _: OptionalMemberExpression =>
        parent match {
          case Some(CallExpression(callee, _)) if (callee == node) => true
          case Some(MemberExpression(obj, _, _, _)) if (obj == node) => true
          case _ => false
        }
      case AssignmentExpression(_, left, _) => false // TODO:
      case LogicalExpression(op, _, _) => false // TODO:
      case Identifier(name) => false // TODO:
      case _ => false
    }
}
