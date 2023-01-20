package mlscript.codegen.ast

import scala.annotation.targetName
import mlscript.{Loc, Located, Term}
import mlscript.codegen.Location

object shorthands:
  /**
    * The type of half-constructed `Node`s without the second parameter list.
    */
  type PartialNode[T <: Node] = (Option[Int], Option[Int], Option[Location]) => T

  extension [T](constructor: (Option[Int], Option[Int], Option[Location]) => T)
    def |>(located: Located): T =
      val (start, end, location) = located.toLoc match
        case Some(Loc(spanStart, spanEnd, _)) => (Some(spanStart), Some(spanEnd), None)
        case None => (None, None, None)
      constructor(start, end, location)
    
    def |>(range: (Located, Located)): T =
      val merged = (range._1.toLoc, range._2.toLoc) match
        case (Some(x), Some(y)) => Some(x ++ y)
        case (Some(x), _) => Some(x)
        case (_, Some(y)) => Some(y)
        case (None, None) => None
      val (start, end, location) = merged match
        case Some(Loc(spanStart, spanEnd, _)) => (Some(spanStart), Some(spanEnd), None)
        case None => (None, None, None)
      constructor(start, end, location)

    def |>(node: Node): T =
      constructor(node.start, node.end, node.location)
    
    @targetName("withLocation")
    def |>(range: (Node, Node)): T =
      val start = range._1.start.flatMap(start1 => range._2.start.map(_.max(start1)))
      val end = range._1.end.flatMap(end1 => range._2.end.map(_.max(end1)))
      // TODO: Merge two `Location`s.
      constructor(start, end, None)

    def |>(nodes: Iterable[Node]): T =
      (nodes.headOption, nodes.lastOption) match
        case (None, None) => constructor.get
        case (Some(head), Some(last)) if head == last => constructor |> head
        case (Some(head), Some(last)) => constructor |> (head, last)
        case _ => mlscript.utils.die

    def get: T = constructor(None, None, None)

  extension (expression: Node with Expression)
    def toStatement: ExpressionStatement =
      ExpressionStatement(expression) |> expression

    def toReturnStatement: ReturnStatement =
      ReturnStatement(Some(expression)) |> expression

  extension (statement: Node with Statement)
    def toBlockStatement: BlockStatement =
      BlockStatement(statement :: Nil) |> statement

    // def ::(blockStatement: BlockStatement): BlockStatement =
    //   BlockStatement(statement :: blockStatement.body) |> (statement, blockStatement)

  def noop: UnaryExpression =
    UnaryExpression(UnaryOperator.Void, NumericLiteral(0)(None, None, None))(None, None, None)

  def makeSimpleAssignment(name: String, expression: Node with Expression): AssignmentExpression =
    AssignmentExpression("=", Identifier(name).get, expression) |> expression

  def makeSingleConstBinding(name: String, expression: Node with Expression): VariableDeclaration =
    VariableDeclaration(
      VariableDeclarationKind.Let,
      VariableDeclarator(Identifier(name).get, Some(expression)).get :: Nil
    ).get

  def makeSingleLetBinding(name: String): VariableDeclaration =
    VariableDeclaration(
      VariableDeclarationKind.Let,
      VariableDeclarator(Identifier(name).get, None).get :: Nil
    ).get

  def makeVoidStatement(expression: Node with Expression): ExpressionStatement =
    ExpressionStatement(
      UnaryExpression(UnaryOperator.Void, expression) |> expression
    ) |> expression

  def makeIfThenElseChain
      (branches: (Node with Expression, List[Node with Statement])*)
      (otherwise: List[Node with Statement]): Node with Statement =
    branches.foldRight[Node with Statement](BlockStatement(otherwise).get) {
      case ((test, consequent), alternate) =>
        IfStatement(test, BlockStatement(consequent).get, Some(alternate)).get
    }

  def makeDisconjunction(expressions: (Node with Expression)*): Node with Expression =
    expressions match
      case Nil =>
        BooleanLiteral(false).get
      case head :: tail =>
        tail.foldLeft(head)(LogicalExpression(LogicalOperator.Or, _, _).get)
