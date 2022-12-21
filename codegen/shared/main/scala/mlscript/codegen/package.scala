package mlscript

package object codegen:
  // type Pos = {
  //   val line: Int
  //   val column: Int
  // }
  // QUESTION: Start from zero or one?
  case class Position(val line: Int, val column: Int):
    def nextLine: Position = Position(line + 1, 0)
    def +(columnOffset: Int): Position = Position(line, column + columnOffset)

  // type LocType = "start" | "end"
  enum LocationType:
    case Start
    case End

  // type Loc = {
  //   val start: Option[Pos]
  //   val end: Option[Pos]
  //   val identifierName: Option[String]
  //   val filename: Option[String]
  // }
  case class Location(
    val start: Option[Position],
    val end: Option[Position],
    val identifierName: Option[String],
    val fileName: Option[String]
  ):
    def apply(locationType: LocationType): Option[Position] =
      locationType match
        case LocationType.Start => start
        case LocationType.End   => end
