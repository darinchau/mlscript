package mlscript.codegen.generator

class Printer(format: Format, map: SourceMap) {
  private var indentLevel: Int = 0

  def indent(): Unit = if (!format.compact && !format.concise) this.indentLevel += 1

  def dedent(): Unit = if (!format.compact && !format.concise) this.indentLevel -= 1
}