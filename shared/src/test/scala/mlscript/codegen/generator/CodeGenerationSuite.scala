package mlscript.codegen.generator

import mlscript.codegen.ast._

class CodeGenerationSuite extends munit.FunSuite:
  private val format = new Format {
    val compact: Boolean = false
    val minified: Boolean = false
    var concise: Boolean = false
    val retainLines: Boolean = false
    val auxiliaryCommentBefore: String = ""
    val auxiliaryCommentAfter: String = ""
    val adjustMultilineComment: Boolean = false
    val retainFunctionParens: Boolean = false
    val indent = "  "
    
    def shouldPrintComment(comment: String): Boolean = false
  }

  private val sourceMap = new SourceMapBuilder(None, None, Left(""))

  test("Code Generation - Base") {
    {
      val res = CodeGenerator(Placeholder(PlaceholderExpectedNode.Statement, Identifier("bar")(None, None, None))(None, None, None), format, sourceMap).generate()
      assertEquals(res.code, "%%bar%%;")
    }
    {
      val res = CodeGenerator(InterpreterDirective("foo")(None, None, None), format, sourceMap).generate()
      assertEquals(res.code, "#!foo")
    }
    {
      val res = CodeGenerator(BlockStatement(List(
        ExpressionStatement(Identifier("foo")(None, None, None))(None, None, None),
        ExpressionStatement(Identifier("bar")(None, None, None))(None, None, None),
        ExpressionStatement(Identifier("baz")(None, None, None))(None, None, None)
      ))(None, None, None), format, sourceMap).generate()
      assertEquals(res.code, "{foo;bar;baz;}")
    }
  }
  test("Code Generation - Classes") {
    
  }
  test("Code Generation - Expression") {
    {
      val res = CodeGenerator(ConditionalExpression(Identifier("foo")(None, None, None),
        Identifier("bar")(None, None, None),
        Identifier("baz")(None, None, None))(None, None, None), format, sourceMap).generate()
      assertEquals(res.code, "foo ? bar : baz")
    }
    {
      val res = CodeGenerator(Decorator(Identifier("dec")(None, None, None))(None, None, None), format, sourceMap).generate()
      assertEquals(res.code, "@dec")
    }
    {
      val res = CodeGenerator(AwaitExpression(Identifier("stuck")(None, None, None))(None, None, None), format, sourceMap).generate()
      assertEquals(res.code, "await stuck")
    }
    {
      val res = CodeGenerator(ThisExpression()(None, None, None), format, sourceMap).generate()
      assertEquals(res.code, "this")
    }
    {
      val res = CodeGenerator(EmptyStatement()(None, None, None), format, sourceMap).generate()
      assertEquals(res.code, ";")
    }
    {
      val res = CodeGenerator(PrivateName(Identifier("rua")(None, None, None))(None, None, None), format, sourceMap).generate()
      assertEquals(res.code, "#rua")
    }
    {
      val res = CodeGenerator(CallExpression(Identifier("foo")(None, None, None),
        List(Identifier("bar")(None, None, None), Identifier("baz")(None, None, None)))(None, None, None), format, sourceMap).generate()
      assertEquals(res.code, "foo(bar, baz)")
    }
  }
  test("Code Generation - Flow") {
    {
      val res = CodeGenerator(ExistsTypeAnnotation()(None, None, None), format, sourceMap).generate()
      assertEquals(res.code, "*")
    }
    {
      val res = CodeGenerator(BooleanLiteralTypeAnnotation(true)(None, None, None), format, sourceMap).generate()
      assertEquals(res.code, "true")
    }
    {
      val res = CodeGenerator(InferredPredicate()(None, None, None), format, sourceMap).generate()
      assertEquals(res.code, "%checks")
    }
  }
  test("Code Generation - JSX") {
    {
      val res = CodeGenerator(JSXFragment(JSXOpeningFragment()(None, None, None),
        JSXClosingFragment()(None, None, None),
        List(JSXText("foo")(None, None, None)))(None, None, None), format, sourceMap).generate()
      assertEquals(res.code, "<>foo</>")
    }
    {
      val res = CodeGenerator(JSXClosingFragment()(None, None, None), format, sourceMap).generate()
      assertEquals(res.code, "</>")
    }
  }
  test("Code Generation - Statement") {
    {
      val res = CodeGenerator(DebuggerStatement()(None, None, None), format, sourceMap).generate()
      assertEquals(res.code, "debugger;")
    }
  }
  test("Code Generation - Types") {
  }
  test("Code Generation - TypeScript") {
    {
      val res = CodeGenerator(TSEnumDeclaration(
        Identifier("foo")(None, None, None),
        List(TSEnumMember(Identifier("bar")(None, None, None))(None, None, None),
          TSEnumMember(Identifier("baz")(None, None, None))(None, None, None))
      )(None, None, None), format, sourceMap).generate()
      assertEquals(res.code, "enum foo {bar,baz,}")
    }
  }
  test("Code Generation - Comprehensive Tests") {
    {
      val res = CodeGenerator(
        File(Program(
          List[Node with Statement](
            ClassDeclaration(
              Identifier("Optional")(None, None, None),
              None,
              ClassBody(List(
                ClassProperty(Identifier("type")(None, None, None))(None, None, None)
              ))(None, None, None)
            )(None, None, None),
            ClassDeclaration(
              Identifier("Some")(None, None, None), Some(Identifier("Optional")(None, None, None)),
              ClassBody(List(
                ClassProperty(Identifier("type")(None, None, None), Some(StringLiteral("\"Some\"")(None, None, None)))(None, None, None),
                ClassProperty(Identifier("value")(None, None, None))(None, None, None),
                ClassMethod(
                  Some(ClassMethodKind.Constructor),
                  Identifier("constructor")(None, None, None),
                  List[Identifier | Node with Pattern | RestElement | TSParameterProperty](Identifier("v")(None, None, None)),
                  BlockStatement(
                    List[Node with Statement](
                      ExpressionStatement(CallExpression(Super()(None, None, None), List())(None, None, None))(None, None, None),
                      ExpressionStatement(AssignmentExpression("=",
                        MemberExpression(ThisExpression()(None, None, None), Identifier("value")(None, None, None))(None, None, None),
                        Identifier("v")(None, None, None))(None, None, None))(None, None, None)
                    )
                  )(None, None, None)
                )(None, None, None)
              ))(None, None, None)
            )(None, None, None),
            ClassDeclaration(
              Identifier("None")(None, None, None), Some(Identifier("Optional")(None, None, None)),
              ClassBody(List(
                ClassProperty(Identifier("type")(None, None, None), Some(StringLiteral("\"None\"")(None, None, None)))(None, None, None)
              ))(None, None, None)
            )(None, None, None),
            FunctionDeclaration(
              Some(Identifier("getOrElse")(None, None, None)),
              List[Identifier | Node with Pattern | RestElement](
                Identifier("opt")(None, None, None),
                Identifier("deflt")(None, None, None)
              ),
              BlockStatement(
                List[Node with Statement](
                  IfStatement(
                    BinaryExpression(
                      BinaryOperator.StrictEqual,
                      MemberExpression(Identifier("opt")(None, None, None), Identifier("type")(None, None, None))(None, None, None),
                      StringLiteral("\"Some\"")(None, None, None)
                    )(None, None, None),
                    ReturnStatement(Some(
                      MemberExpression(Identifier("opt")(None, None, None), Identifier("value")(None, None, None))(None, None, None)
                    ))(None, None, None),
                    Some(ReturnStatement(Some(Identifier("deflt")(None, None, None)))(None, None, None))
                  )(None, None, None)
                )
              )(None, None, None)
            )(None, None, None),
            VariableDeclaration(
              VariableDeclarationKind.Const,
              List(
                VariableDeclarator(Identifier("foo")(None, None, None), Some(
                  NewExpression(Identifier("Some")(None, None, None),
                    List[Node with Expression | SpreadElement | JSXNamespacedName | ArgumentPlaceholder](
                      NumericLiteral(42)(None, None, None)
                    ))(None, None, None)
                ))(None, None, None),
                VariableDeclarator(Identifier("bar")(None, None, None), Some(
                  NewExpression(Identifier("None")(None, None, None),
                    List[Node with Expression | SpreadElement | JSXNamespacedName | ArgumentPlaceholder]())(None, None, None)
                ))(None, None, None)
              )
            )(None, None, None),
            ExpressionStatement(
              CallExpression(
                MemberExpression(Identifier("console")(None, None, None), Identifier("log")(None, None, None))(None, None, None),
                List[Node with Expression | SpreadElement | JSXNamespacedName | ArgumentPlaceholder](
                  CallExpression(Identifier("getOrElse")(None, None, None),
                    List[Node with Expression | SpreadElement | JSXNamespacedName | ArgumentPlaceholder](
                    Identifier("foo")(None, None, None), NumericLiteral(0)(None, None, None)
                  ))(None, None, None)
                )
              )(None, None, None)
            )(None, None, None),
            ExpressionStatement(
              CallExpression(
                MemberExpression(Identifier("console")(None, None, None), Identifier("log")(None, None, None))(None, None, None),
                List[Node with Expression | SpreadElement | JSXNamespacedName | ArgumentPlaceholder](
                  CallExpression(Identifier("getOrElse")(None, None, None),
                    List[Node with Expression | SpreadElement | JSXNamespacedName | ArgumentPlaceholder](
                    Identifier("bar")(None, None, None), NumericLiteral(0)(None, None, None)
                  ))(None, None, None)
                )
              )(None, None, None)
            )(None, None, None)
          ),
          sourceFile = ""
        )(None, None, None))(None, None, None), format, sourceMap
      ).generate()
      assertEquals(res.code, "class Optional {type;}class Some extends Optional {type = \"Some\";value;constructor(v ) {super();this.value = v;}}class None extends Optional {type = \"None\";}function getOrElse(opt, deflt ) {if (opt.type === \"Some\") {return opt.value;} else return deflt;} const foo = new Some(42),bar = new None();console.log(getOrElse(foo, 0));console.log(getOrElse(bar, 0));")
    }
  }
