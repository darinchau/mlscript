package mlscript.codegen

package object sourcemap:
  enum SourceMapSegment:
    val outputColumn: Int

    case Absolute(outputColumn: Int)
    case Relative(outputColumn: Int, sourceIndex: Int, originalPosition: Position, nameIndex: Option[Int])

  object SourceMapSegment:
    def apply(outputColumn: Int): SourceMapSegment.Absolute = SourceMapSegment.Absolute(outputColumn)
    def apply(outputColumn: Int, sourceIndex: Int, originalLine: Int, originalColumn: Int): SourceMapSegment.Relative =
      SourceMapSegment.Relative(outputColumn, sourceIndex, Position(originalLine, originalColumn), None)
    def apply(outputColumn: Int, sourceIndex: Int, originalLine: Int, originalColumn: Int, nameIndex: Int): SourceMapSegment.Relative =
      SourceMapSegment.Relative(outputColumn, sourceIndex, Position(originalLine, originalColumn), Some(nameIndex))
    def apply(outputColumn: Int, sourceIndex: Int, originalLine: Int, originalColumn: Int, nameIndex: Option[Int]): SourceMapSegment.Relative =
      SourceMapSegment.Relative(outputColumn, sourceIndex, Position(originalLine, originalColumn), nameIndex)

  type SourceMapLine = List[SourceMapSegment]

  type SourceMapMappings = List[SourceMapLine]
