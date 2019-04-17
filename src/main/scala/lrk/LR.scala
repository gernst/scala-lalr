package lrk

import scala.collection.mutable

import lrk.util.Stack

case class Token(symbol: Terminal, text: String)

object LR {
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

    val todo = mutable.Queue[State]()
    val states = mutable.Buffer[State]() // XXX: see whether caching by core is worthwile for larger grammars

    val start = Item(Start, List(), List(grammar.start), Set(End))
    val init = new State(mutable.Set(start), null, grammar)

    todo enqueue init

    while (!todo.isEmpty) {
      val state = todo.dequeue

      /* Try to merge this state with an existing one, max. one candidate */
      val that = states find (_.core == state.core)

      that match {
        case Some(that) if that canMerge state =>
          /* Merge items */
          that.kernel ++= state.kernel
          that.items ++= state.items
          that.merged += state

          /* Fix transitions from the previous state */
          if (state.prev != null) {
            for ((symbol, `state`) <- state.prev.transitions) {
              state.prev.transitions += (symbol -> that)
            }
          }

        case _ =>
          number += 1
          state.number = number
          states += state
          todo ++= state.successors
      }
    }

    for (state <- states)
      assert(state.table != null)

    (init, states)
  }

  def reduce(a: Stack[Any], rindex: List[Int], apply: Any) = (rindex, apply) match {
    case (List(), f: Any) => f
    case (List(i0), f: Function1[Any, Any] @unchecked) =>
      val a0 = a(i0)
      f(a(i0))
    case (List(i0, i1), f: Function2[Any, Any, Any] @unchecked) =>
      val a0 = a(i0)
      val a1 = a(i1)
      f(a(i0), a(i1))
    case (List(i0, i1, i2), f: Function3[Any, Any, Any, Any] @unchecked) => f(a(i0), a(i1), a(i2))
  }

  def parse(in: Iterable[Token], init: State): Any = {
    val states = new Stack[State]()
    val results = new Stack[Any]()

    val iter = in.iterator
    def next() = if (iter.hasNext) iter.next else Token(End, null)
    var token = next()

    states push init

    while (true) {
      val state = states.top
      // println("in state: " + state.number)
      // println("results:  " + results)

      val action = state.table action token.symbol

      action match {
        case Accept =>
          assert(results.length == 1)
          return results.pop

        case Reject =>
          sys.error("unexpected symbol: " + token.symbol)

        case Shift(state) =>
          // println("shift " + token)
          results push token.text
          states push state
          token = next()

        case Reduce(rule) =>
          val arity = rule.rhs.length
          // println("reduce " + rule)
          assert(rule.rindex forall (_ < arity))
          val res = reduce(results, rule.rindex, rule.apply) // rule apply args.reverse

          states drop arity
          results drop arity

          val next = states.top.table goto rule.lhs

          states push next
          results push res
      }
    }
  }
}