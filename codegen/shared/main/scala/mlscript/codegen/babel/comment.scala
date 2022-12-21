package mlscript.codegen.babel

import mlscript.codegen.{Position, Location}

trait BaseComment:
  val value: String
  val start: Option[Int]
  val end: Option[Int]
  val loc: Option[Location]
  val ignore: Option[Boolean]
  val kind: CommentKind

enum CommentKind:
  case Block
  case Line

enum CommentSkipNewLine:
  case Default
  case All
  case Leading
  case Trailing

enum CommentType:
  case Leading
  case Inner
  case Trailing
