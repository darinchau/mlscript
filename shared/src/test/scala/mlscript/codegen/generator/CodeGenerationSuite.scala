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
      assertEquals(res.code, "foo(bar, baz, )")
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
                ClassProperty(Identifier("value")(None, None, None))(None, None, None)
              ))(None, None, None)
            )(None, None, None),
            ClassDeclaration(
              Identifier("None")(None, None, None), Some(Identifier("Optional")(None, None, None)),
              ClassBody(List(
                ClassProperty(Identifier("type")(None, None, None), Some(StringLiteral("\"None\"")(None, None, None)))(None, None, None)
              ))(None, None, None)
            )(None, None, None)
          ),
          sourceFile = ""
        )(None, None, None))(None, None, None), format, sourceMap
      ).generate()
      assertEquals(res.code, "class Optional {type;}class Some extends Optional {type = \"Some\";value;}class None extends Optional {type = \"None\";}")
    }
  }
