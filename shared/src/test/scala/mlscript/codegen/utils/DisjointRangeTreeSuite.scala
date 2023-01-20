package mlscript.codegen.utils

class CodeGenerationSuite extends munit.FunSuite:
  test("Basic") {
    val tree = DisjointRangeTree.from(
      (0, 4),
      (9, 10),
      (200, 30)
    )
  }
