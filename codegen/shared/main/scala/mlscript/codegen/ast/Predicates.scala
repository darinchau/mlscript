package mlscript.codegen.ast

object Predicates:
  extension (node: Node)
    def isIdentifier: Boolean =
      node match
        case _: Identifier => true
        case _ => false
    def isFunction: Boolean =
      node match
        case _: Node with Function => true
        case _ => false
    def isBlockStatement: Boolean =
      node match
        case _: BlockStatement => true
        case _ => false
