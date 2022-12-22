package mlscript.codegen.sourcemap

import scala.collection.immutable.Seq
import java.nio.charset.StandardCharsets
import scala.annotation.tailrec

object encode:
  def apply(decoded: SourceMapMappings): String =
    // Begin states
    var outputColumn: Int = 0
    var sourceIndex: Int = 0
    var originalPosition: (Int, Int) = (0, 0)
    var nameIndex: Int = 0
    decoded.iterator.map { line =>
      // Reset the output column.
      outputColumn = 0
      line.iterator.map {
        case SourceMapSegment.Absolute(outputColumnValue) =>
          val result = encode(outputColumnValue, outputColumn)
          outputColumn = outputColumnValue
          result
        case SourceMapSegment.Relative(
          outputColumnValue,
          sourceIndexValue,
          originalPositionValue,
          nameIndexOptionValue
        ) =>
          val buffer = new StringBuilder()
          buffer ++= encode(outputColumnValue, outputColumn)
          outputColumn = outputColumnValue
          buffer ++= encode(sourceIndexValue, sourceIndex)
          sourceIndex = sourceIndexValue
          buffer ++= encode(originalPositionValue.line, originalPosition._1)
          buffer ++= encode(originalPositionValue.column, originalPosition._2)
          originalPosition = (originalPositionValue.line, originalPositionValue.column)
          nameIndexOptionValue.foreach { nameIndexValue =>
            buffer ++= encode(nameIndexValue, nameIndex)
            nameIndex = nameIndexValue
          }
          buffer.toString
      }.mkString(",")
    }.mkString(";")

  private def encode(segmentValue: Int, stateValue: Int): String =
    Base64VLQ.encode(segmentValue - stateValue)
