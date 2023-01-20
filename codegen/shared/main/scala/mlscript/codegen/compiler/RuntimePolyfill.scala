package mlscript.codegen.compiler

import mlscript.codegen.ast._
import mlscript.codegen.ast.shorthands._

abstract class RuntimePolyfill(val name: String):
  def declare(name: String): List[Node with Statement]
  def hasFunctionDeclaration: Boolean = false
  def hasClassDeclaration: Boolean = false

object RuntimePolyfill:
  import mlscript.codegen.ast.snippets._

  val error: RuntimePolyfill = new RuntimePolyfill("error") {
    def declare(name: String): List[Node & Statement] =
      FunctionDeclaration(
        Some(Identifier(name).get),
        AssignmentPattern(Identifier("message").get, StringLiteral("unexpected runtime error").get).get :: Nil,
        ThrowStatement(
          NewExpression(Identifier("Error").get, Identifier("message").get :: Nil).get
        ).get.toBlockStatement
      ).get :: Nil
    override def hasFunctionDeclaration: Boolean = true
  }

  val withConstruct: RuntimePolyfill = new RuntimePolyfill("withConstruct") {
    def declare(name: String): List[Node & Statement] =
      val target = Identifier("target").get
      val fields = Identifier("fields").get
      val clone = Identifier("clone").get
      FunctionDeclaration(
        Some(Identifier(name).get),
        target :: fields :: Nil,
        BlockStatement(
          makeIfThenElseChain(
            (
              makeDisconjunction(
                makeTypeOfTest(target, StringLiteral("string").get).get,
                makeTypeOfTest(target, StringLiteral("number").get).get,
                makeTypeOfTest(target, StringLiteral("boolean").get).get,
                makeTypeOfTest(target, StringLiteral("bigint").get).get,
                makeTypeOfTest(target, StringLiteral("symbol").get).get
              ),
              // return Object.assign(target, fields);
              ReturnStatement(Some(mock.Object.assign(target, fields))).get :: Nil
            ),
            (
              makeDisconjunction(
                makeInstanceOfTest(target, Identifier("String").get).get,
                makeInstanceOfTest(target, Identifier("Number").get).get,
                makeInstanceOfTest(target, Identifier("Boolean").get).get,
                makeInstanceOfTest(target, Identifier("BigInt").get).get
              ),
              // return Object.assign(target.valueOf(), target, fields);
              mock.Object.assign(makeValueOf(target).get, target, fields).toReturnStatement :: Nil
            ),
            (
              mock.Array.isArray(target),
              List(
                // const clone = {};
                VariableDeclaration(
                  VariableDeclarationKind.Const,
                  VariableDeclarator(clone, Some(ObjectExpression(Nil).get)).get :: Nil
                ).get,
                // for (const key in target) { clone[key] = target[key]; }
                makeCopyObjectProperties(target, clone),
                // for (const key in fields) { clone[key] = fields[key]; }
                makeCopyObjectProperties(fields, clone),
                clone.toReturnStatement
              )
            ),
            (
              // target === null || target === undefined
              makeDisconjunction(
                BinaryExpression(BinaryOperator.StrictEqual, target, NullLiteral().get).get,
                BinaryExpression(BinaryOperator.StrictEqual, target, Identifier("undefined").get).get,
              ),
              // return Object.assign({}, fields);
              mock.Object.assign(ObjectExpression(Nil).get, fields).toReturnStatement :: Nil
            )
          )(
            // const copy = Object.assign({}, target, fields);
            VariableDeclaration(
              VariableDeclarationKind.Const,
              VariableDeclarator(
                Identifier("copy").get,
                Some(mock.Object.assign(ObjectExpression(Nil).get, target, fields))
              ).get :: Nil
            ).get ::
              // Object.setPrototypeOf(copy, Object.getPrototypeOf(target));
              ExpressionStatement(
                mock.Object.setPrototypeOf(
                  Identifier("copy").get,
                  mock.Object.getPrototypeOf(target)
                )
              ).get ::
              // return copy;
              Identifier("copy").get.toReturnStatement :: Nil
          ) :: Nil
        ).get
      ).get :: Nil
    override def hasFunctionDeclaration: Boolean = true
  }
  