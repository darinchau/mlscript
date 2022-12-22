package mlscript.codegen.sourcemap

/**
  * Source Map Revision 3
  * See https://sourcemaps.info/spec.html for the specification.
  *
  * @param file An optional name of the generated code that this source map is associated with.
  * @param names A list of symbol names used by the `mappings` entry.
  * @param sourceRoot An optional source root, useful for relocating source files on a server or
  *        removing repeated values in the `sources` entry.
  *        This value is prepended to the individual entries in the “source” field.
  * @param sources A list of original sources used by the `mappings` entry.
  * @param sourcesContent An optional list of source content, useful when the `source` cannot be hosted.
  *        The contents are listed in the same order as the `sources` field.
  *        `None` may be used if some original sources should be retrieved by name.
  * @param mappings The decoded mapping data.
  */
class SourceMap(
  file: Option[String],
  names: Vector[String],
  sourceRoot: Option[String],
  sources: Vector[Option[String]],
  sourcesContent: Vector[Option[String]],
  mappings: SourceMapMappings,
):
  def toJSON: String = ???