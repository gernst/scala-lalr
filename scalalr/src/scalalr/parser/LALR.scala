package scalalr.parser

import scala.collection.mutable

import scalalr.Leaf
import scalalr.Node
import scalalr.Parser
import scalalr.Position
import scalalr.Range
import scalalr.Token
import scalalr.Tree
import scalalr.WithRules
import scalalr.util.NonTerminal
import scalalr.util.Symbol
import scalalr.util.Stack
import scalalr.util.Terminal
import scalalr.util.Timer
import scalalr.util.Digraph
import scalalr.util.Numbering

sealed trait Action

case object Accept extends Action {
  override def toString = "accept"
}

case object Reject extends Action {
  override def toString = "reject"
}

case class Shift(next: State) extends Action {
  override def toString = "shift " + next
}

case class Reduce(rule: Rule) extends Action {
  override def toString = "reduce " + rule
}

case class Table(action: Map[Terminal, Action], goto: Map[NonTerminal, State]) {
}

case class Item(lhs: NonTerminal, rdone: List[Symbol], todo: List[Symbol]) {
  def canReduce = todo.isEmpty
  def done = rdone.reverse
  override def toString = lhs + " -> " + done.mkString(" ") + " . " + todo.mkString(" ")
}

object State extends Numbering {

}

case class State(kernel: Set[Item]) extends State.numbered {
  var items: Set[Item] = Set()
  var transitions: Map[Symbol, State] = Map()

  override def toString = "state " + index

  def dump = {
    var res = this + "\n"
    for (item <- kernel) {
      res += "  " + item + " *\n"
    }
    for (item <- items if !(kernel contains item)) {
      res += "  " + item + "\n"
    }
    for ((symbol, next) <- transitions) {
      symbol match {
        case t: Terminal =>
          res += "  shift " + t + " to " + next + "\n"
        case n: NonTerminal =>
          res += "  goto " + n + ": " + next + "\n"
      }
    }
    res
  }
}

object LALR {
  var debug = false

  def translate(init: Parser[_]): Grammar = {
    val top = init match {
      case init: WithRules => init
      case _ => throw new IllegalArgumentException("not a top-level parser: " + init)
    }

    val parsers = Digraph.fix[WithRules](List(top), _.other)
    val rules = parsers flatMap (_.rules)

    Grammar(top.symbol, rules.toList)
  }

  def unfold(item: Item, grammar: Grammar) = item match {
    case Item(_, _, (lhs: NonTerminal) :: rest) =>
      val rules = grammar index lhs
      rules map (_.item)
    case _ =>
      Nil
  }

  def next(items: Iterable[Item]): Map[Symbol, State] = {
    val symbols = for (Item(lhs, rdone, first :: rest) <- items)
      yield first

    val transitions = for (symbol <- symbols) yield {
      val kernel = for (Item(lhs, rdone, `symbol` :: rest) <- items)
        yield Item(lhs, symbol :: rdone, rest)
      symbol -> State(kernel.toSet)
    }

    transitions.toMap
  }

  def close(state: State, grammar: Grammar) {
    val items = Digraph.fix[Item](state.kernel, unfold(_, grammar))
    state.items = items.toSet
    state.transitions = next(items)
  }

  /**
   * Compacting LR(k) parser automaton generator.
   *
   * Losely based on http://david.tribble.com/text/lrk_parsing.html
   */
  def states(grammar: Grammar): (State, Set[State]) = {
    val start = Item(Start, List(), List(grammar.start))
    val init = State(Set(start))

    def succ(state: State) = {
      close(state, grammar)
      state.transitions.values
    }

    val result = Digraph.fix[State](List(init), succ)
    (init, result.toSet)
  }

  def parse(in: Iterator[Token], init: State, annotate: Boolean = false): Any = ???

  /* def reduce(a: Int => Any, rindex: List[Int], apply: Any) = (rindex, apply) match {
    case (List(), f: Function0[Any] @unchecked) => f()
    case (List(i0), f: Function1[Any, Any] @unchecked) => f(a(i0))
    case (List(i1, i0), f: Function2[Any, Any, Any] @unchecked) => f(a(i0), a(i1))
    case (List(i2, i1, i0), f: Function3[Any, Any, Any, Any] @unchecked) => f(a(i0), a(i1), a(i2))
  }

  def parse(in: Iterator[Token], init: State, annotate: Boolean = false): Any = {
    val states = new Stack[State]()
    val results = new Stack[Any]()

    var range = Range.empty
    var position = Position.empty

    def unpack(n: Int) = results(n).asInstanceOf[Tree].value
    def get(i: Int, n: Int) = if (annotate) unpack(n - i - 1) else results(n - i - 1)
    def next() = if (in.hasNext) in.next else Token(End, null, range.after, position)
    var token = next()

    states push init

    while (true) {
      val state = states.top
      if (debug) println("in state: " + state.number)
      if (debug) println("results:  " + results)

      val action = state.table action token.symbol

      action match {
        case Accept =>
          assert(results.length == 1)
          return results.pop

        case Reject =>
          if (debug) println(state.dump)
          throw UnexpectedSymbol(token.symbol, token.position)

        case Shift(state) =>
          if (debug) println("shift " + token)
          val result = token.text

          val value = if (annotate) {
            range = token.range
            position = token.position
            Leaf(result, range, position)
          } else {
            result
          }

          results push value
          states push state
          token = next()

        case Reduce(rule) =>
          val arity = rule.rhs.length
          if (debug) println("reduce " + rule)
          assert(rule.rindex forall (_ < arity))

          val result = reduce(get(_, arity), rule.rindex, rule.apply)

          val value = if (annotate) {
            val args = List.tabulate(arity)(i => results(arity - 1 - i).asInstanceOf[Tree])
            val node = Node(result, args)
            if (rule.collapse) node.collapse
            else node
          } else {
            result
          }

          states drop arity
          results drop arity

          val next = states.top.table goto rule.lhs

          states push next
          results push value
      }
    }
  } */
}
