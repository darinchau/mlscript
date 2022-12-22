package mlscript.codegen.sourcemap

import scala.collection.mutable.Buffer
import mlscript.codegen.Position
import scala.annotation.tailrec

class Builder(val file: Option[String], val sourceRoot: Option[String]):
  val names: SetArray = new SetArray()
  val sources: SetArray = new SetArray()
  val sourcesContent: Buffer[Option[String]] = Buffer.empty
  val mappings: Buffer[Buffer[SourceMapSegment]] = Buffer.empty

  def toSourceMap: SourceMap =
    new SourceMap(
      file,
      names.keys.toVector,
      sourceRoot,
      sources.keys.iterator.map(Some.apply).toVector,
      sourcesContent.toVector,
      mappings.iterator.map(_.toList).toList
    )

  // I assume this is rarely used.
  def getAllMappings: Buffer[Mapping] =
    mappings.iterator.zipWithIndex.flatMap { case (line, i) =>
      line.iterator.zipWithIndex.map { case (segment, j) =>
        val generated = Position(i + 1, segment.outputColumn)
        segment match
          case SourceMapSegment.Absolute(_) => Mapping(generated)
          case SourceMapSegment.Relative(_, sourceIndex, originalPosition, nameIndex) =>
            Mapping(generated, sources.getKey(sourceIndex), originalPosition, nameIndex.map(names.getKey))
      }
    }.toBuffer

  def getLine(index: Int): Buffer[SourceMapSegment] =
    while (mappings.length <= index) mappings += Buffer.empty
    mappings(index)

  def getColumnIndex(line: Buffer[SourceMapSegment], generatedColumn: Int): Int =
    val index = line.lastIndexWhere(generatedColumn >= _.outputColumn)
    if index < 0 then 0 else index + 1 // To insert after the position.
  
  def addMapping(skipable: Boolean, mapping: Mapping): Unit =
    val line = getLine(mapping.generated.line)
    val index = getColumnIndex(line, mapping.generated.column)
    mapping.source match
      case None => addSegment(skipable, line, index, mapping.generated.column)
      case Some(source) => addSegment(skipable, line, index, mapping.generated.column, source)
    
  def addSegment(skipable: Boolean, line: Buffer[SourceMapSegment], index: Int, generatedColumn: Int): Unit =
    if (!skipable || !isSkipSourceLess(line, index)) line.insert(index, SourceMapSegment(generatedColumn))

  def addSegment(skipable: Boolean, line: Buffer[SourceMapSegment], index: Int, generatedColumn: Int, source: MappingSource): Unit =
    if (!skipable || !isSkipSourceLess(line, index)) {
      val sourceIndex = sources.get(source.fileName).getOrElse {
        // Should we raise errors or just return -1 as what we do with `nameIndex`?
        // We should look for the specification.
        throw new Exception(s"unknown source file name: \"${source.fileName}\"")
      }
      val nameIndex = source.name.map(names.get(_).getOrElse(-1))
      line.insert(index, SourceMapSegment(generatedColumn, sourceIndex, source.original.line, source.original.column, nameIndex))
    }

  def isSkipSource(line: Buffer[SourceMapSegment], index: Int, sourceIndex: Int, sourcePosition: Position, nameIndex: Int): Boolean =
    // A source/named segment at the start of a line gives position at that genColumn
    if index == 0 then false else line(index - 1) match
      // If the previous segment is sourceless, then we're transitioning to a source.
      case _: SourceMapSegment.Absolute => false
      // If the previous segment maps to the exact same source position, then this segment doesn't
      // provide any new position information.
      case SourceMapSegment.Relative(_, prevSourceIndex, previousPosition, prevNameIndex) =>
        prevSourceIndex == sourceIndex && previousPosition == sourcePosition && nameIndex == prevNameIndex.getOrElse(-1)

  def isSkipSourceLess(line: Buffer[SourceMapSegment], index: Int): Boolean =
    // The start of a line is already sourceless, so adding a sourceless segment to the beginning
    // doesn't generate any useful information.
    index == 0 || (line(index - 1) match
      // If the previous segment is also sourceless, then adding another sourceless segment doesn't
      // genrate any new information. Else, this segment will end the source/named segment and point to
      // a sourceless position, which is useful.
      case _: SourceMapSegment.Absolute => true
      case _: SourceMapSegment.Relative => false)

  def setSourceContent(sourceFileName: String, content: Option[String]): Unit =
    val index = sources.put(sourceFileName)
    sourcesContent.insert(index, content)
