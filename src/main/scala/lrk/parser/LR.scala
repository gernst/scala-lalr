package lrk.parser

import scala.collection.mutable

import lrk.Leaf
import lrk.Node
import lrk.Parser
import lrk.Range
import lrk.Token
import lrk.Tree
import lrk.WithRules
import lrk.util.NonTerminal
import lrk.util.Stack
import lrk.util.Terminal

sealed trait Action

case object Accept extends Action {
  override def toString = "accept"
}

case object Reject extends Action {
  override def toString = "reject"
}

case class Shift(next: State) extends Action {
  override def toString = "shift " + next.number
}

case class Reduce(rule: Rule) extends Action {
  override def toString = "reduce " + rule
}

case class Table(action: Terminal => Action, goto: NonTerminal => State) {
}

object LR {
  var debug = false
  
  def translate(init: Parser[_]): Grammar = {
    val rules = mutable.ListBuffer[Rule]()

    val done = mutable.Set[WithRules]()
    val todo = mutable.Queue[WithRules]()

    val top = init match {
      case init: WithRules => init
      case _ => sys.error("not a top-level parser: " + init)
    }
    todo enqueue top

    while (!todo.isEmpty) {
      val parser = todo.dequeue

      if (!(done contains parser)) {
        done += parser
        for (other <- parser.other)
          todo enqueue other
        rules ++= parser.rules
      }
    }

    Grammar(top.symbol, rules.toList)
  }

  /**
   * Compacting LR(k) parser automaton generator.
   *
   * Losely based on http://david.tribble.com/text/lrk_parsing.html
   */
  def states(grammar: Grammar): (State, Seq[State]) = {
    var number = 0

    val incomplete = mutable.Queue[State]()
    val states = mutable.Buffer[State]() // XXX: see whether caching by core is worthwile for larger grammars

    val start = Item(Start, List(), List(grammar.start), Set(End))
    val init = new State(mutable.Set(start), null, grammar)

    incomplete enqueue init
    init.computeItems()

    while (!incomplete.isEmpty) {
      val todo = mutable.Queue[State]()

      if (!incomplete.isEmpty) {
        val state = incomplete.dequeue
        state.computeTransitions()
        todo ++= state.succ
        states += state
      }

      for (state <- todo) {
        state.computeItems()
        val that = states find (_.core == state.core)

        that match {
          case Some(that) if state canMerge that =>
            val changed = that merge state

            if (changed) {
              incomplete += that
              // that.recompute()
              // todo ++= state.succ
            }
          // states -= that
          // assert(!changed)

          case _ =>
            number += 1
            state.number = number
            incomplete += state
            // states += state

            // state.computeTransitions()
            // todo ++= state.succ
        }
      }
    }

    for (state <- states)
      assert(state.table != null)

    (init, states)
  }

  def reduce(a: Int => Any, rindex: List[Int], apply: Any) = (rindex, apply) match {
    case (List(), f: Any) => f
    case (List(i0), f: Function1[Any, Any] @unchecked) => f(a(i0))
    case (List(i1, i0), f: Function2[Any, Any, Any] @unchecked) => f(a(i0), a(i1))
    case (List(i2, i1, i0), f: Function3[Any, Any, Any, Any] @unchecked) => f(a(i0), a(i1), a(i2))
  }

  def parse(in: Iterator[Token], init: State, annotate: Boolean = false): Any = {
    val states = new Stack[State]()
    val results = new Stack[Any]()

    var pos = 0
    def unpack(n: Int) = results(n).asInstanceOf[Tree].value
    def get(i: Int, n: Int) = if (annotate) unpack(n - i - 1) else results(n - i - 1)
    def next() = if (in.hasNext) in.next else Token(End, null, Range(pos, 0))
    var token = next()

    states push init

    while (true) {
      val state = states.top
      if(debug) println("in state: " + state.number)
      if(debug) println("results:  " + results)

      val action = state.table action token.symbol

      action match {
        case Accept =>
          assert(results.length == 1)
          return results.pop

        case Reject =>
          if(debug) println(state.dump)
          sys.error("unexpected symbol: " + token.symbol)

        case Shift(state) =>
          if(debug) println("shift " + token)
          val result = token.text

          val value = if (annotate) {
            val range = token.range
            pos = range.end
            Leaf(result, range)
          } else {
            result
          }

          results push value
          states push state
          token = next()

        case Reduce(rule) =>
          val arity = rule.rhs.length
          if(debug) println("reduce " + rule)
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
  }
}