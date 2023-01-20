package mlscript.codegen.compiler

import scala.collection.mutable.{Buffer, Map as MutMap, Set as MutSet}
import mlscript._
import mlscript.utils.algorithms._
import mlscript.codegen.ast._
import mlscript.codegen.ast.shorthands._

case class ControlFlowNode(
  val statements: List[Node with mlscript.codegen.ast.Statement],
  val expression: Node with Expression
):
  def isPure: Boolean = statements.isEmpty

  def map(worker: (Node with Expression) => Node with Expression): ControlFlowNode =
    copy(expression = worker(expression))

  def combine(node: ControlFlowNode)(worker: (Node with Expression, Node with Expression) => ControlFlowNode): ControlFlowNode =
    val result = worker(expression, node.expression)
    ControlFlowNode(statements ::: node.statements ::: result.statements, result.expression)

  def combine(node1: ControlFlowNode, node2: ControlFlowNode)(worker: (Node with Expression, Node with Expression, Node with Expression) => ControlFlowNode): ControlFlowNode =
    val result = worker(expression, node1.expression, node2.expression)
    ControlFlowNode(statements ::: node1.statements ::: node2.statements ::: result.statements, result.expression)

  def toBlockStatement(resultIdentifier: Identifier): BlockStatement =
    BlockStatement(statements :+ ExpressionStatement(AssignmentExpression("=", resultIdentifier, expression).get).get).get

  def concat(node: ControlFlowNode): ControlFlowNode =
    node.copy(statements = statements ::: (makeVoidStatement(expression) :: node.statements))

  def +:(prefix: Node with mlscript.codegen.ast.Statement): ControlFlowNode =
    copy(statements = prefix :: statements)

  def ++:(prefix: List[Node with mlscript.codegen.ast.Statement]): ControlFlowNode =
    copy(statements = prefix ::: statements)

class Compiler(options: CompilerOptions):
  type StatementNode = Node with mlscript.codegen.ast.Statement

  private val topLevelScope: Scope = new Scope("Root", None)

  // Declare the by-name error function.
  topLevelScope.declareVariable("error", true, Some(RuntimePolyfill.error))

  private val enabledRuntimePolyfills = MutMap.empty[String, Identifier]
  private val emittedRuntimePolyfills = MutSet.empty[String]

  private def useRuntimePolyfill(polyfill: RuntimePolyfill): Identifier =
    enabledRuntimePolyfills.getOrElseUpdate(RuntimePolyfill.withConstruct.name, {
      val name = topLevelScope.allocate()
      RuntimePolyfill.withConstruct.declare(name)
      Identifier(name).get
    })

  extension (`var`: Var)
    def toIdentifier: Identifier =
      Identifier(`var`.name) |> `var`

  extension (node: Node with Expression)
    def toControlFlowNode: ControlFlowNode = ControlFlowNode(Nil, node)

  extension (nodes: Iterator[ControlFlowNode])
    def combine: (List[Node with mlscript.codegen.ast.Statement], List[Node with Expression]) =
      nodes.foldLeft[(List[Node with mlscript.codegen.ast.Statement], List[Node with Expression])]((Nil, Nil)) { case ((statements, expressions), node) =>
        (statements ::: node.statements, expressions :+ node.expression)
      }

  private def sanitizePropertyName(name: Var): Identifier | StringLiteral =
    Identifier(name.name) |> name // TODO: Complete the `IdentifierHelpers` first.
    
  private def compilePattern(t: Term)(using scope: Scope): Identifier | Node with Pattern =
    t match
      case Var(name) =>
        val symbol = scope.declareVariable(name)
        Identifier(symbol.runtimeName) |> t
      case Rcd(fields) =>
        ObjectPattern(fields.map {
          case (key, Fld(_, _, propertyAlias: Var)) =>
            // Declare the alias to the property in the scope.
            val patternSymbol = scope.declareVariable(propertyAlias.name)
            ObjectProperty(
              sanitizePropertyName(key),
              Identifier(patternSymbol.runtimeName) |> propertyAlias
            ).get
          case (key, Fld(_, _, subTerm)) => 
            ObjectProperty(sanitizePropertyName(key), compilePattern(subTerm)).get
        }) |> t
      case Asc(t, _) =>
        compilePattern(t)
      case _: Lit => Identifier("_") |> t
      case Bra(_, trm) =>
        compilePattern(trm)
      case Tup(fields) =>
        ArrayPattern(fields.map {
          case (_, Fld(_, _, _: Lit)) => None
          case (_, Fld(_, _, t)) => Some(compilePattern(t))
        }) |> t
      // Others are not supported yet.
      case TyApp(base, _) =>
        compilePattern(base)
      case _: Lam | _: App | _: Sel | _: Let | _: Blk | _: Bind | _: Test | _: With | _: CaseOf | _: Subs | _: Assign
          | If(_, _) | New(_, _) | _: Splc =>
        throw new CompilerException(s"term ${t.describe} is not a valid pattern")

  private def compileParameters(t: Term, scope: Scope): List[Identifier | Node with Pattern] =
    t match
      case Tup(params) =>
        params.map { case _ -> Fld(_, _, p) => compilePattern(p)(using scope) }
      case _ =>
        throw new CompilerException(s"term $t is not a valid parameter list")

  private def compileVariable(name: String, located: mlscript.Located, isCallee: Boolean)(using scope: Scope): ControlFlowNode =
    scope.resolve(name) match
      case Some(Symbol.Variable(_, runtimeName, isByValueRec, isFunction)) =>
        if (isByValueRec.getOrElse(false) && !isFunction)
          throw new CompilerException(s"unguarded recursive use of by-value binding $name")
        val identifier = Identifier(runtimeName) |> located
        (if (isByValueRec.isEmpty && !isFunction)
          CallExpression(identifier, Nil) |> located
        else
          identifier).toControlFlowNode
      case Some(Symbol.Builtin(_, isByName, runtimePolyfill)) =>
        val polyfillName = useRuntimePolyfill(runtimePolyfill)
        (if isByName && !isCallee then
          CallExpression(polyfillName, Nil) |> located
        else
          polyfillName).toControlFlowNode
      case Some(Symbol.Constructor(_, runtimeName, Cls)) =>
        val temporaryName = scope.allocate()
        val declaration = makeSingleConstBinding(
          temporaryName,
          ArrowFunctionExpression(
            Identifier("x").get :: Nil,
            NewExpression(Identifier(runtimeName) |> located, Identifier("x").get :: Nil).get,
            false,
            true
          ).get
        )
        ControlFlowNode(declaration :: Nil, Identifier(temporaryName) |> located)
      case Some(Symbol.Constructor(_, runtimeName, Trt)) =>
        MemberExpression(Identifier(runtimeName).get, Identifier("build").get).get.toControlFlowNode
      case None if options.allowUnresolvedSymbols =>
        (Identifier(name) |> located).toControlFlowNode
      case None =>
        throw new CompilerException(s"unresolved symbol $name")

  private def compileIfThenElse(
    test: Node with Expression,
    consequent: ControlFlowNode,
    alternate: ControlFlowNode,
    located: mlscript.Located,
  )(using scope: Scope): ControlFlowNode =
    if (consequent.isPure && alternate.isPure)
      (ConditionalExpression(test, consequent.expression, alternate.expression) |> located).toControlFlowNode
    else
      val resultName = scope.allocate()
      val resultIdentifier = Identifier(resultName).get
      val resultDeclaration = makeSingleLetBinding(resultName)
      val ifStatement = IfStatement(
        test,
        consequent.toBlockStatement(resultIdentifier),
        Some(alternate.toBlockStatement(resultIdentifier))
      ) |> located
      ControlFlowNode(resultDeclaration :: ifStatement :: Nil, resultIdentifier)


  private def compileApplication(term: App)(using Scope): ControlFlowNode =
    term match
      // Binary expressions
      case App(App(Var(op), Tup((None -> Fld(_, _, lhs)) :: Nil)), Tup((None -> Fld(_, _, rhs)) :: Nil))
          if JSBinary.operators contains op =>
        compileTerm(lhs).combine(compileTerm(rhs)) { case (left, right) =>
          val expression = (BinaryOperator.from(op) match
            case Some(operator) => BinaryExpression(operator, left, right)
            case None => LogicalOperator.from(op) match
              case Some(operator) => LogicalExpression(operator, left, right)
              case None => throw new CompilerException(s"unknown binary operator: $op")) |> term
          expression.toControlFlowNode
        }
      // If-expressions
      case App(App(App(Var("if"), test), consequent), alternate) =>
        val testNode = compileTerm(test)
        testNode.statements ++: compileIfThenElse(
          testNode.expression,
          compileTerm(consequent),
          compileTerm(alternate),
          term
        )
      // Function invocation
      case App(trm, Tup(args)) =>
        val callee = trm match
          case Var(nme) => compileVariable(nme, trm, true)
          case _ => compileTerm(trm)
        val (argumentStatements, arguments) = args.iterator.map {
          case (_, Fld(_, _, arg)) => compileTerm(arg)
        }.combine
        ControlFlowNode(
          callee.statements ::: argumentStatements,
          CallExpression(callee.expression, arguments) |> term
        )
      case _ => throw CompilerException(s"ill-formed application ${term.describe}")

  // You should pass `scope` explicitly to reduce errors.
  private def compileFunctionBody(term: Term, scope: Scope): BlockStatement =
    val node = compileTerm(term)(using scope)
    BlockStatement(node.statements :+ node.expression.toReturnStatement) |> term
    
  private def compileArrowFunctionBody(term: Term, scope: Scope): BlockStatement | Node with Expression =
    val node = compileTerm(term)(using scope)
    if node.isPure then node.expression else compileFunctionBody(term, scope)

  private def compileLiteral(term: Lit): Identifier | Node with mlscript.codegen.ast.Literal =
    term match
      case IntLit(value) =>
        if value.isValidInt then
          NumericLiteral(value.toInt) |> term
        else
          throw new CompilerException(s"the integer value $value cannot be natively represented in JavaScript")
      case DecLit(value) =>
        val integralPart = value.toBigInt
        if integralPart == value then
          BigIntLiteral(value.toString) |> term
        else if value.isExactDouble then
          NumericLiteral(value.toDouble) |> term
        else
          throw new CompilerException(s"the decimal value $value cannot be natively represented in JavaScript")
      case StrLit(value) =>
        StringLiteral(value) |> term
      case UnitLit(true) =>
        Identifier("undefined") |> term
      case UnitLit(false) =>
        NullLiteral() |> term

  private def compileFunctionDeclaration(name: Var, args: Term, body: Term)(using scope: Scope): PartialNode[FunctionDeclaration] =
    val functionScope = scope.derive(s"Function ${name.name}")
    FunctionDeclaration(
      Some(name.toIdentifier),
      compileParameters(args, functionScope),
      compileFunctionBody(body, functionScope)
    )

  private def compileTerm(term: Term)(using scope: Scope): ControlFlowNode =
    term.desugaredTerm.getOrElse(term) match
      case Var(name) =>
        compileVariable(name, term, isCallee = false)
      case Lam(parameters, body) =>
        val functionScope = scope.derive(s"Lambda @ ${term.toLoc}")
        val patterns = compileParameters(parameters, functionScope)
        val functionBody = compileFunctionBody(body, functionScope)
        ControlFlowNode(Nil, ArrowFunctionExpression(patterns, functionBody, false, false) |> term)
      case term: App =>
        compileApplication(term)
      case Rcd(fields) =>
        val statementBuffer = Buffer.empty[StatementNode]
        val expression = ObjectExpression(fields.map { case (key, Fld(_, _, value)) =>
          val ControlFlowNode(statements, expression) = compileTerm(value)
          statementBuffer ++= statements
          ObjectProperty(Identifier(key.name) |> key, expression) |> (key, value) 
        }) |> term
        ControlFlowNode(statementBuffer.toList, expression)
      case Sel(receiver, fieldName) =>
        compileTerm(receiver).map {
          // TODO: Check if `fieldName` is an valid property name.
          MemberExpression(_, Identifier(fieldName.name) |> fieldName) |> term
        }
      case Let(true, name, lam @ Lam(args, body), rest) =>
        val functionDeclaration = compileFunctionDeclaration(name, args, body) |> lam
        functionDeclaration +: compileTerm(rest)
      case Let(true, Var(name), _, _) =>
        throw new CompilerException(s"recursive non-function definition $name is not supported")
      case Let(false, name, value, body) =>
        val valueNode = compileTerm(value)
        val symbol = scope.declareVariable(name.name)
        val declaration = VariableDeclaration(
          VariableDeclarationKind.Const,
          VariableDeclarator(
            Identifier(symbol.runtimeName) |> name,
            Some(valueNode.expression)
          ).get :: Nil
        ).get
        val bodyNode = compileTerm(body)
        val statements = valueNode.statements ::: (declaration :: bodyNode.statements) 
        ControlFlowNode(statements, bodyNode.expression)
      case Blk(stmts) =>
        val blockScope = scope.derive(s"Block @ ${term.toLoc}")
        val compiledNodes = stmts.iterator.flatMap(_.desugared._2).map {
          case term: Term => compileTerm(term)(using blockScope)
          case _: Def | _: TypeDef | _: NuFunDef =>
            throw new CompilerException("unsupported definitions in blocks")
        }.toList
        compiledNodes.lastOption match
          case None => ControlFlowNode(Nil, noop) // We need an empty status for control flow nodes.
          case Some(lastNode) =>
            val resultName = scope.allocate()
            val initStatements = compiledNodes.init.flatMap {
              case ControlFlowNode(statements, expression) => 
                statements :+ makeVoidStatement(expression)
            }
            val saveStatement = makeSimpleAssignment(resultName, lastNode.expression).toStatement
            ControlFlowNode({
              makeSingleLetBinding(resultName) ::
                BlockStatement((initStatements ::: lastNode.statements) :+ saveStatement).get ::
                Nil
            }, Identifier(resultName).get)
      case CaseOf(scrutinee, Wildcard(default)) =>
        compileTerm(scrutinee).concat(compileTerm(default))
      case CaseOf(scrutinee, cases) =>
        val scrutineeNode = compileTerm(scrutinee)
        val scrutineeIdentifier = scrutineeNode.expression match
          case identifier: Identifier => identifier
          case _ =>
            val scrutineeName = scope.allocate()
            Identifier(scrutineeName).get
        scrutineeNode.statements ++: compileCaseBranch(scrutineeIdentifier, cases)
      case term: Lit =>
        compileLiteral(term).toControlFlowNode
      case Asc(term, _) =>
        compileTerm(term)
      case With(basement, surplus @ Rcd(fields)) =>
        val withFunctionName = useRuntimePolyfill(RuntimePolyfill.withConstruct)
        compileTerm(basement).combine(compileTerm(surplus)) { (basement, surplus) =>
          (CallExpression(withFunctionName, basement :: surplus :: Nil) |> term).toControlFlowNode
        }
      case Bra(_, term) =>
        compileTerm(term)
      case Tup(elements) =>
        val (statements, expressions) = elements.iterator.map {
          case (_, Fld(_, _, term)) => compileTerm(term)
        }.combine
        ControlFlowNode(
          statements,
          ArrayExpression(expressions.map(Some.apply)) |> term
        )
      case Subs(array, index) =>
        compileTerm(array).combine(compileTerm(index)) { (array, index) =>
          (MemberExpression(array, index, true) |> term).toControlFlowNode
        }
      case Assign(lhs, rhs) =>
        compileTerm(lhs) match
          case ControlFlowNode(statements, leftHandSide: Node with LVal) =>
            val rhsNode = compileTerm(rhs)
            val assignment = ExpressionStatement(
              AssignmentExpression("=", leftHandSide, rhsNode.expression) |> term
            ) |> term
            ControlFlowNode((statements ::: rhsNode.statements) :+ assignment, noop)
          case _ =>
            throw CompilerException(s"the left-hand side value was compiled into a non-LVal")
      case _: If =>
        throw CompilerException(s"ultimate conditional expression has not been desugared")
      case New(None, TypingUnit(Nil)) =>
        (ObjectExpression(Nil) |> term).toControlFlowNode
      case New(Some((className: TypeName) -> Tup(args)), TypingUnit(Nil)) =>
        val calleeNode = compileVariable(className.name, className, true)
        val argumentNodes = args.iterator.map {
          case (_, Fld(_, _, arg)) => compileTerm(arg)
        }.combine
        ControlFlowNode(
          calleeNode.statements ::: argumentNodes._1,
          CallExpression(calleeNode.expression, argumentNodes._2) |> term
        )
      case New(_, TypingUnit(_)) =>
        throw new CompilerException("custom class body is not supported yet")
      case _: Bind | _: Test | _: TyApp | _: Splc =>
        throw new CompilerException(s"code generation for ${term.describe} has not been implemented yet")

  private def compileCaseBranch(
    scrutinee: Node with Expression,
    branch: CaseBranches
  )(using scope: Scope): ControlFlowNode =
    def unfoldCaseBranches(
      branch: CaseBranches,
      accumulated: List[(Node with Expression, ControlFlowNode, Case)]
    ): (List[(Node with Expression, ControlFlowNode, Case)], ControlFlowNode) =
      branch match
        case branch @ Case(pattern, body, rest) =>
          unfoldCaseBranches(rest, accumulated :+ (compileCasePattern(scrutinee, pattern) |> branch, compileTerm(body), branch))
        case NoCases =>
          (accumulated, (snippets.makeUnreachableCase |> branch).toControlFlowNode)
        case Wildcard(body) =>
          (accumulated, compileTerm(body))
    val (branches, defaultNode) = unfoldCaseBranches(branch, Nil)
    val resultName = scope.allocate()
    val resultIdentifier = Identifier(resultName).get
    val resultDeclaration = makeSingleLetBinding(resultName)
    // Fold the branches into a chain of if-then-else statements.
    val ifStatement = branches.foldRight[StatementNode](defaultNode.toBlockStatement(resultIdentifier)) {
      case ((test, consequent, theCase), alternate) =>
        IfStatement(test, consequent.toBlockStatement(resultIdentifier), Some(alternate)) |> theCase
    }
    ControlFlowNode(resultDeclaration :: ifStatement :: Nil, resultIdentifier)

  private def compileCasePattern(scrutinee: Node with Expression, pattern: SimpleTerm)(using scope: Scope): PartialNode[Node with Expression] =
    pattern match
      case Var("int") =>
        // Number.isInteger(scrutinee)
        CallExpression(
          MemberExpression(Identifier("Number").get, Identifier("isInteger").get).get,
          scrutinee :: Nil
        )
      case Var("bool") =>
        // typeof scrutinee === "boolean"
        snippets.makeTypeOfTest(scrutinee, StringLiteral("boolean") |> pattern)
      case Var("true") =>
        // !!scrutinee
        UnaryExpression(
          UnaryOperator.LogicalNot,
          UnaryExpression(UnaryOperator.LogicalNot, scrutinee).get
        )
      case Var("false") =>
        // !scrutinee
        UnaryExpression(UnaryOperator.LogicalNot, scrutinee)
      case Var("string") =>
        // typeof scrutinee === "string"
        snippets.makeTypeOfTest(scrutinee, StringLiteral("string") |> pattern)
      case Var(name) =>
        scope.resolve(name) match
          case Some(Symbol.Constructor(_, runtimeName, Cls)) =>
            BinaryExpression(BinaryOperator.InstanceOf, scrutinee, Identifier(runtimeName) |> pattern)
          case Some(Symbol.Constructor(_, runtimeName, Trt)) =>
            CallExpression(
              MemberExpression(Identifier(runtimeName) |> pattern, Identifier("is").get).get,
              scrutinee :: Nil
            )
          case Some(Symbol.Builtin(_, _, runtimePolyfill)) if runtimePolyfill.hasClassDeclaration =>
            val className = useRuntimePolyfill(runtimePolyfill)
            BinaryExpression(BinaryOperator.InstanceOf, scrutinee, className)
          case Some(Symbol.Builtin(_, _, _)) =>
            throw new CompilerException(s"$name does not declare any classes")
          case Some(_: Symbol.Variable) =>
            throw new CompilerException(s"cannot match $scrutinee against a variable")
          case None =>
            throw new CompilerException(s"unresolved constructor name $name")
      case term: Lit =>
        BinaryExpression(BinaryOperator.StrictEqual, scrutinee, compileLiteral(term))

  private def compileDeclarations(typeDefs: List[TypeDef]): List[Node with Declaration] = ???

  protected def declareTypeDefs(typeDefs: List[TypeDef]): (List[Symbol.Constructor], List[Symbol.Constructor]) = {
    val traits = Buffer.empty[Symbol.Constructor]
    val classes = Buffer.empty[Symbol.Constructor]
    typeDefs.foreach {
      case TypeDef(kind @ (Trt | Cls), TypeName(name), _, _, _, _, _) =>
        traits += topLevelScope.declareConstructor(name, kind)
      case TypeDef(Als, _, _, _, _, _, _) =>
        throw new CompilerException("Code generation for type alias is not supported.")
      case TypeDef(Nms, _, _, _, _, _, _) =>
        throw new CompilerException("Code generation for namespaces is not supported.")
    }
    (traits.toList, classes.toList)
  }

  /**
    * Find the base class of the given class.
    */
  def resolveBaseClass(ty: Type): Option[Symbol.Constructor] =
    val baseClasses = ty.collectTypeNames.flatMap { name =>
      topLevelScope.resolve(name) match
        case Some(sym @ Symbol.Constructor(_, _, Cls)) => Some(sym)
        case Some(sym @ Symbol.Constructor(_, _, Trt)) => None
        case Some(_) =>
          throw new CompilerException(s"cannot inherit from variables" )
        case None =>
          throw new CompilerException(s"undeclared type name $name when resolving base classes")
    }
    if (baseClasses.lengthIs > 1)
      throw new CompilerException(
        s"cannot have ${baseClasses.length} base classes: " +
        baseClasses.map { _.lexicalName }.mkString(", ")
      )
    else
      baseClasses.headOption

  protected def sortClassSymbols(classSymbols: List[(Symbol.Constructor, Type)]): Iterable[(Symbol.Constructor, Option[Symbol.Constructor])] =
    // Cache base classes for class symbols.
    val baseClasses = Map.from(classSymbols.iterator.flatMap { case (derivedClass, classBaseType) =>
      resolveBaseClass(classBaseType).map(derivedClass -> _)
    })
    val ordering = new Ordering[Symbol.Constructor] {
      def compare(x: Symbol.Constructor, y: Symbol.Constructor): Int =
        x.lexicalName compareTo y.lexicalName
    }
    val sorted: Iterable[Symbol.Constructor] = try topologicalSort(baseClasses, classSymbols.map(_._1))(using ordering) catch {
      case e: CyclicGraphError => throw new CompilerException("cyclic inheritance detected")
    }
    // Their base classes might be class symbols defined in previous translation
    // units. So we filter them out here.
    sorted.flatMap(sym => if (classSymbols.iterator.map(_._1).contains(sym)) Some(sym -> baseClasses.get(sym)) else None)

  def compileTraitDeclaration(traitSymbol: Symbol.Constructor, baseType: Type)(scope: Scope): VariableDeclaration = ???

  def compile(program: Pgrm): File =
    val (diags, (typeDefs, otherStmts)) = program.desugared
    val (traitSymbols, classSymbols) = declareTypeDefs(typeDefs)
    // val defStmts = 
    //   traitSymbols.map { translateTraitDeclaration(_)(topLevelScope) } ++
    //   sortClassSymbols(classSymbols).map { case (derived, base) =>
    //     translateClassDeclaration(derived, base)(topLevelScope)
    //   }.toList
    ???
