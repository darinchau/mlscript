package mlscript.codegen.generator

trait Format {
  val compact: Boolean
  val minified: Boolean
  var concise: Boolean
  val retainLines: Boolean
  val adjustMultilineComment: Boolean
  val retainFunctionParens: Boolean
  val indent: String
  
  def shouldPrintComment(comment: String): Boolean
}
