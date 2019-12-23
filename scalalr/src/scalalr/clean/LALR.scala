package scalalr.clean

import scala.collection.mutable

sealed trait Symbol

case class Terminal(name: String) extends Symbol {
  override def toString = name
}

case class NonTerminal(name: String) extends Symbol {
  def ->(that: Symbol*) = Rule(this, List(that: _*))
  override def toString = name
}

object Start extends NonTerminal("@")
object End extends Terminal("$")

case class Rule(lhs: NonTerminal, rhs: List[Symbol]) {
  def pair = (lhs, rhs)
}

case class Grammar(start: NonTerminal, rules: List[Rule]) {
  def index = LALR.group(rules map (_.pair))
}

case class Core(lhs: NonTerminal, rdone: List[Symbol], todo: List[Symbol]) {
  def done = rdone.reverse
  override def toString = lhs + " -> " + done.mkString(" ") + " . " + todo.mkString(" ")
}

case class Item(lhs: NonTerminal, rdone: List[Symbol], todo: List[Symbol], look: Terminal) {
  def done = rdone.reverse
  def core = Core(lhs, rdone, todo)

  def shift = todo match {
    case Nil => None
    case first :: rest => Some(first -> Item(lhs, first :: rdone, rest, look))
  }

  override def toString = lhs + " -> " + done.mkString(" ") + " . " + todo.mkString(" ") + " / " + look
}

case class Look(tail: List[Symbol], look: Set[Terminal]) {

}

case class State(kernel: Set[Item]) {
  def core = kernel map (_.core)
  var prev: State = null
  var items = Set[Item]()
  var next = Map[Symbol, State]()
  override def toString = items.mkString("\n")
}

class LALR(grammar: Grammar) {
  import LALR._

  val rules = {
    grammar.index
  }

  val top = {
    Item(Start, Nil, List(grammar.start), End)
  }

  def init = {
    state(Set(top), null)
  }

  def state(kernel: Set[Item], prev: State) = {
    val state = State(kernel)
    state.prev = prev
    state.items = close(kernel)
    state
  }

  def transitions(items: Set[Item]): Map[Symbol, Set[Item]] = {
    group(items flatMap (_.shift))
  }

  def merge(state: State, diff: Set[Item]) {
    state.items ++= diff

    for ((symbol, succ) <- transitions(diff)) {
      val next = merge(state next symbol, succ)
    }
  }

  def states(init: State) = {
    val todo = mutable.Queue[State]()
    val delay = mutable.Queue[(State, Set[Item])]()
    var result = Map[Set[Core], State]()

    todo += init
    result += (init.core -> init)

    while (!todo.isEmpty) {
      val prev = todo.dequeue

      for ((symbol, kernel) <- transitions(prev.items)) {
        val that = state(kernel, prev)

        result get that.core match {
          case None =>
            val next = that
            prev.next += symbol -> next
            result += next.core -> next
            todo += next

          case Some(next) =>
            prev.next += symbol -> next
            val diff = that.items -- next.items
            delay += ((next, diff))
        }
      }
    }
    
    for((state, diff) <- delay) {
      merge(state, diff)
    }

    result.values
  }

  object close extends Fix[Item] {
    def succ(item: Item) = item match {
      case Item(_, rdone, (lhs: NonTerminal) :: rest, look) =>
        for (
          rhs <- rules(lhs);
          term <- first(rest, look)
        ) yield Item(lhs, Nil, rhs, term)
      case _ =>
        List()
    }
  }

  object first extends Digraph[Look, Terminal] {
    def apply(tail: List[Symbol], look: Terminal): Set[Terminal] = {
      val start = Look(tail, Set(look))
      val result = apply(Set(start))
      result(start)
    }

    def init(s: Look): Iterable[Terminal] = s match {
      case Look(Nil, look) => look
      case Look((t: Terminal) :: _, _) => Set(t)
      case Look((n: NonTerminal) :: _, _) => Set()
    }

    def succ(s: Look): Iterable[Look] = s match {
      case Look(Nil, look) => List()
      case Look((t: Terminal) :: _, _) => List()
      case Look((n: NonTerminal) :: rest, look) =>
        for (rhs <- rules(n)) yield {
          Look(rhs ++ rest, look)
        }
    }
  }
}

object LALR {
  val S = NonTerminal("S")
  val E = NonTerminal("E")
  val T = NonTerminal("T")
  val n = Terminal("n")
  val p = Terminal("+")
  val lp = Terminal("(")
  val rp = Terminal(")")

  val start = S
  val rules = List(
    S -> (E),
    E -> (T),
    E -> (lp, E, rp),
    T -> (n),
    T -> (p, T),
    T -> (T, p, n))

  val grammar = Grammar(start, rules)

  def main(args: Array[String]) {
    object lalr extends LALR(grammar)
    val init = lalr.init
    for (state <- lalr.states(init)) {
      println(state)
    }
  }

  def group[A, B](xs: Iterable[(A, B)]): Map[A, Set[B]] = {
    var result = Map[A, Set[B]]()
    for ((a, b) <- xs) {
      if (!(result contains a))
        result += a -> Set[B]()
      result += a -> (result(a) + b)
    }
    result
  }

  trait Fix[X] {
    def succ(x: X): Iterable[X]

    def apply(init: Iterable[X]): Set[X] = {
      import scala.collection.mutable

      val todo = mutable.Queue[X]()
      var result = Set[X]()
      todo ++= init

      while (!todo.isEmpty) {
        val x = todo.dequeue
        if (!(result contains x)) {
          result += x
          todo ++= succ(x)
        }
      }

      result
    }
  }

  trait Digraph[X, A] {
    def init(x: X): Iterable[A]
    def succ(x: X): Iterable[X]

    def apply(states: Iterable[X]): (X => Set[A]) = {
      import scala.collection.mutable

      val stack = mutable.Stack[X]()
      val depth = mutable.Map[X, Int]()
      val result = mutable.Map[X, Set[A]]()

      for (x <- states) {
        start(x)
      }

      def start(x: X) {
        if (!(depth contains x))
          traverse(x)
      }

      def traverse(x: X) {
        stack push x
        val d = stack.length

        depth(x) = d
        result(x) = Set()
        result(x) ++= init(x)

        for (y <- succ(x)) {
          start(y)

          depth(x) = Math.min(depth(x), depth(y))
          result(x) ++= result(y)
        }

        if (depth(x) == d) {
          do {
            val y = stack.top
            depth(y) = Int.MaxValue
            result(y) = result(x)
          } while (stack.pop() != x)
        }
      }

      result
    }
  }
}