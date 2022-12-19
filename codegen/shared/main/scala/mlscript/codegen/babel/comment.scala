package mlscript.codegen.babel

trait BaseComment {
  val value: String
  val ignore: Option[Boolean]
}

case class SourcePosition(line: Int, column: Int)

case class SourceLocation(start: SourcePosition, end: SourcePosition)
