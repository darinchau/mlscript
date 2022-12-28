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
    
    def shouldPrintComment(comment: String): Boolean = false
  }

  private val sourceMap = new SourceMapBuilder(None, None, Left(""))

  test("Code Generation - Base") {
    {
      val res = CodeGenerator(InterpreterDirective("foo")(None, None, None), format, sourceMap).generate()
      assertEquals(res.code, "#!foo")
    }
  }
  test("Code Generation - Classes") {
    
  }
  test("Code Generation - Expression") {
    {
      val res = CodeGenerator(ThisExpression()(None, None, None), format, sourceMap).generate()
      assertEquals(res.code, "this")
    }
    {
      val res = CodeGenerator(EmptyStatement()(None, None, None), format, sourceMap).generate()
      assertEquals(res.code, ";")
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
  }
