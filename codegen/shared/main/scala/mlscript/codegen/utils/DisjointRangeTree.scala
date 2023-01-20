package mlscript.codegen.utils

import scala.collection.immutable.Range

final case class DisjointRangeTreeNode(
  val range: Range.Inclusive,
  val left: Option[DisjointRangeTreeNode],
  val right: Option[DisjointRangeTreeNode],
):
  def contains(value: Int): Boolean =
    range.contains(value) &&
      left.map(_.contains(value)).getOrElse(true) &&
      right.map(_.contains(value)).getOrElse(true)

object DisjointRangeTreeNode:
  def build(elements: Vector[(Range.Inclusive)]): Option[DisjointRangeTreeNode] =
    elements.headOption match
      case None => None
      case Some(sole) if elements.length == 1 =>
        Some(DisjointRangeTreeNode(sole, None, None))
      case _ =>
        val middle = elements.length / 2
        val left = build(elements.take(middle))
        val right = build(elements.drop(middle))
        val start = elements.minBy(_.start).start
        val end = elements.maxBy(_.end).end
        Some(DisjointRangeTreeNode(start to end, left, right))

class DisjointRangeTree(private[this] val root: Option[DisjointRangeTreeNode]):
  def contains(value: Int): Boolean = root.map(_.contains(value)).getOrElse(false)

object DisjointRangeTree:
  def from(tuples: IterableOnce[(Int, Int)]): DisjointRangeTree =
    val ranges = tuples.iterator.map { case (a, b) => a to b }.toVector
    DisjointRangeTree(DisjointRangeTreeNode.build(ranges))
