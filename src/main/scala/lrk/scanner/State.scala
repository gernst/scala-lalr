package lrk.scanner

import scala.collection.mutable

import lrk.Regex
import lrk.util.Terminal

case class Item(lhs: Terminal, rhs: Regex) {
  def first = rhs.first
  def derive(c: Letter) = Item(lhs, rhs derive c)
  def accepts = rhs.isNullable
}

class State(val items: List[Item], val prev: State) {
  var number = 0

  def successors = {
    transitions map { case (_, next) => next }
  }

  def first = items.foldLeft(Letters.empty) {
    case (cs, item) => cs | item.first
  }

  def accepts = {
    items exists (_.accepts)
  }

  def canAccept = {
    for (item <- items if item.accepts)
      yield item.lhs
  }

  def derive(c: Letter) = {
    new State(items map (_ derive c), this)
  }

  lazy val transitions = {
    val result = mutable.LongMap[State]()
    for (c <- first) {
      result(c) = derive(c)
    }
    result
  }
  
    override def toString = "state " + number

  def dump = {
    var res = this + "\n"
    res += "  previous: " + prev + "\n"
    for (item <- items) {
      res += "  " + item
      res += "\n"
    }
    for ((letter, next) <- transitions) {
      res += "  with " + Letter.fmt(letter.toInt) + " to " + next + "\n"
    }
    res
  }
}