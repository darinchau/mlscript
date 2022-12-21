package mlscript.codegen.babel

trait BaseComment {
  val value: String
  val start: Option[Int]
  val end: Option[Int]
  val loc: Option[SourceLocation]
  val ignore: Option[Boolean]
  val tp: "CommentBlock" | "CommentLine"
}

case class SourcePosition(line: Int, column: Int)

case class SourceLocation(start: SourcePosition, end: SourcePosition)

enum COMMENT_SKIP_NEWLINE {
  case DEFAULT, ALL, LEADING, TRAILING
}

enum COMMENT_TYPE {
  case LEADING, INNER, TRAILING
}
