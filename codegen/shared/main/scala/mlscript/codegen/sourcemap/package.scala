package mlscript.codegen

object sourcemap {
  case class SourceMapV3(
    file: Option[String],
    names: Seq[String],
    sourceRoot: Option[String],
    sources: Seq[Option[String]],
    sourcesContent: Seq[Option[String]],
    version: Int
  )

  case class EncodedSourceMap(
    mappings: String,
    file: Option[String],
    names: Seq[String],
    sourceRoot: Option[String],
    sources: Seq[Option[String]],
    sourcesContent: Seq[Option[String]],
    version: Int
  ) extends SourceMapV3

  case class DecodedSourceMap(
    mappings: Seq[Seq[SourceMapSegment]],
    file: Option[String],
    names: Seq[String],
    sourceRoot: Option[String],
    sources: Seq[Option[String]],
    sourcesContent: Seq[Option[String]],
    version: Int
  ) extends SourceMapV3

  case class Pos(line: Int, column: Int)

  enum Mapping:
    case MappingA(generated: Pos)
    case MappingB(generated: Pos, source: String, original: Pos, name: Option[String])

}