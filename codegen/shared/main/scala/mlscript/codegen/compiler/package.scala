package mlscript.codegen

package object compiler:
  enum JavaScriptDialect:
    case JavaScript(revision: Option[Int] = None)
    case TypeScript(declarationOnly: Boolean = true)

  final case class CompilerOptions(
    val allowUnresolvedSymbols: Boolean = false,
    val dialect: JavaScriptDialect
  )

  final class CompilerException(message: String) extends Exception(message)
