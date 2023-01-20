package mlscript.codegen.compiler

import mlscript.ObjDefKind

enum Symbol:
  val lexicalName: String

  case Variable(
    val lexicalName: String,
    val runtimeName: String,
    val isByValueRec: Option[Boolean],
    val isFunction: Boolean
  )
  case Builtin(
    val lexicalName: String,
    val isByName: Boolean,
    val runtimePolyfill: RuntimePolyfill
  )
  case Constructor(val lexicalName: String, val runtimeName: String, val kind: ObjDefKind)
