package scalalr

import scala.annotation.tailrec

case class Position(line: Int, column: Int)

object Position {
  def find(pos: Int, path: List[Tree]) = {
    down(pos, up(pos, path))
  }

  // find nearest ancestor in path that contains a given position
  @tailrec
  def up(pos: Int, path: List[Tree]): List[Tree] = path match {
    case Nil =>
      Nil
    case at :: path =>
      if (at.range contains pos) path
      else up(pos, path)
  }

  // find most specific child that ranges over a position
  @tailrec
  def down(pos: Int, path: List[Tree]): List[Tree] = path.head match {
    case (at: Leaf) =>
      path
    case Node(_, children) =>
      children.find(_.range contains pos) match {
        case None =>
          path
        case Some(child) =>
          down(pos, child :: path)
      }
  }
}

/* Scala's range is broken.
 * Edsger W. Dijkstra: Why numbering should start at zero, 1982.
 */
case class Range(start: Int, length: Int) {
  def isEmpty = {
    length == 0
  }

  def end = {
    start + length
  }

  def +(shift: Int) = {
    Range(start + shift, length)
  }

  def |(that: Range) = {
    val start = Math.min(this.start, that.start)
    val end = Math.max(this.end, that.end)
    Range(start, end - start)
  }

  def contains(pos: Int) = {
    start <= pos && pos < end
  }

  def subsetOf(that: Range) = {
    (that contains start) && (that contains end)
  }
}

object Range {
  val empty = Range(0, 0)
}

sealed trait Tree {
  def value: Any
  def range: Range
  def children: List[Tree]
}

case class Leaf(value: Any, range: Range, position: Position) extends Tree {
  def children = Nil
}

case class Node(value: Any, children: List[Tree]) extends Tree {
  def range = children.foldLeft(Range.empty)(_ | _.range)
  def collapse = Node(value, children flatMap (_.children))
}
