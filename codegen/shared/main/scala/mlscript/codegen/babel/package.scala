package mlscript.codegen.babel

import mlscript.codegen.generator.Printer
import mlscript.codegen.Location
import mlscript.codegen.babel.BaseComment

trait BaseNode(
  val compact: Boolean,
  val loc: Option[Location],
  val extra: Option[Map[String, String]], // the original type is Record<string, unknown>...
  val leadingComments: Option[Array[BaseComment]],
  val innerComments: Option[Array[BaseComment]],
  val trailingComments: Option[Array[BaseComment]]
) {
  def print(printer: Printer, parent: BaseNode): Unit
}

package object babel {
  
}
