package mlscript.codegen.babel

import mlscript.codegen.{Position, Location}

trait BaseComment:
  val value: String
  val start: Option[Int]
  val end: Option[Int]
  val loc: Option[Location]
  val ignore: Option[Boolean]

enum CommentSkipNewLine:
  case Default
  case All
  case Leading
  case Trailing

enum CommentType:
  case Leading
  case Inner
  case Trailing

enum PrintCommentHint:
  case Skip
  case Allow
  case Defer

// TODO: Fix other fields
case class CommentBlock(value: String) extends BaseComment {
  val start: Option[Int] = None
  val end: Option[Int] = None
  val loc: Option[Location] = None
  val ignore: Option[Boolean] = None
}

case class CommentLine(value: String) extends BaseComment {
  val start: Option[Int] = None
  val end: Option[Int] = None
  val loc: Option[Location] = None
  val ignore: Option[Boolean] = None
}
