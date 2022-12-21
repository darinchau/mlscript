package mlscript.codegen.generator

trait Format {
  val compact: Boolean
  val minified: Boolean
  val concise: Boolean
  val retainLines: Boolean
  val auxiliaryCommentBefore: String
  val auxiliaryCommentAfter: String
  val adjustMultilineComment: Boolean
}
