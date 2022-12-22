package mlscript.codegen.generator

import scala.collection.mutable.Buffer
import mlscript.codegen.Position
import mlscript.codegen.sourcemap.{Builder, Mapping, SourceMap => SourceMapResult}

class SourceMapBuilder(sourceFileName: Option[String], sourceRoot: Option[String], code: Either[String, Map[String, String]]):
  private val builder: Builder = new Builder(sourceFileName, sourceRoot)

  def getSourceMap: SourceMapResult = builder.toSourceMap

  /**
    * Mark the current generated position without a source position.
    * This means that we insert a mapping to nothing.
    */
  def mark(generated: Position): Unit =
    builder.addMapping(true, Mapping(generated))

  /**
    * Mark the current generated position with a source position.
    * May also be passed null line/column values to insert a mapping to nothing.
    */
  def mark(generated: Position, original: Position, fileName: String, identifierName: Option[String]): Unit =
    builder.addMapping(true, Mapping(generated, fileName, original, identifierName))
