package scalalr.clean

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

case class Item(lhs: NonTerminal, rdone: List[Symbol], todo: List[Symbol], look: Set[Terminal]) {
  def done = rdone.reverse
  def core = Core(lhs, rdone, todo)
  override def toString = lhs + " -> " + done.mkString(" ") + " . " + todo.mkString(" ") + look.mkString(" { ", " ", " }")
}

case class State(kernel: Set[Item], items: Set[Item]) {
  def core = kernel map (_.core)
}

class LALR(grammar: Grammar) {
  import LALR._

  val rules = {
    grammar.index
  }

  val lift = {
    Item(Start, List(), List(grammar.start), Set(End))
  }

  val init = {
    val kernel = Set(lift)
    val items = close(kernel)
    State(kernel, items)
  }

  object first extends Digraph[(List[Symbol], Set[Terminal]), Terminal] {
    def of(symbols: List[Symbol], look: Set[Terminal]) = {
      val seed = (symbols, look)
      val result = apply(List(seed))
      result(seed)
    }

    def init(s: (List[Symbol], Set[Terminal])) = s match {
      case (Nil, look) => look
      case ((t: Terminal) :: _, _) => Set(t)
      case ((n: NonTerminal) :: _, _) => Set()
    }

    def succ(s: (List[Symbol], Set[Terminal])) = s match {
      case (Nil, look) => List()
      case ((t: Terminal) :: _, _) => List()
      case ((n: NonTerminal) :: rest, look) =>
        for (rhs <- rules(n)) yield {
          (rhs ++ rest, look)
        }
    }
  }

  object close extends Fix[Item] {
    def succ(item: Item) = item match {
      case Item(_, rdone, (lhs: NonTerminal) :: rest, look) =>
        val follow = first of (rest, look)
        for (rhs <- rules(lhs)) yield {
          Item(lhs, Nil, rhs, follow)
        }
      case _ =>
        Set()
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
    lalr.init.items map println
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
          result(x) = result(x) union result(y)
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