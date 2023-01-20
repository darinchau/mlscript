package mlscript.codegen.compiler

import scala.collection.mutable.{Map as MutMap, Set as MutSet}
import mlscript.ObjDefKind

class Scope(name: String, enclosing: Option[Scope]):
  private val symbols = MutMap.empty[String, Symbol]
  private val reserved = MutSet.empty[String]

  def declareVariable(
    lexicalName: String,
    isByName: Boolean = false,
    runtimePolyfill: Option[RuntimePolyfill] = None
  ): Symbol.Variable = ???

  def declareConstructor(lexicalName: String, kind: ObjDefKind): Symbol.Constructor = ???

  def resolve(name: String): Option[Symbol] = ???

  def allocate(): String = ???

  def derive(name: String): Scope = new Scope(name, Some(this))
