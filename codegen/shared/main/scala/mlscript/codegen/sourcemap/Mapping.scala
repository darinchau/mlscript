package mlscript.codegen.sourcemap

import mlscript.codegen.Position

case class MappingSource(fileName: String, original: Position, name: Option[String])

/**
  * A high-level mapping from the source code to the generated code.
  */
case class Mapping(generated: Position, source: Option[MappingSource])

object Mapping:
  def apply(generated: Position): Mapping = Mapping(generated, None)
  def apply(generated: Position, fileName: String, original: Position, name: Option[String]): Mapping =
    Mapping(generated, Some(MappingSource(fileName, original, name)))
