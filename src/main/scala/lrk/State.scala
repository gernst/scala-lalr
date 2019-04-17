package lrk

import scala.collection.mutable

case class Item(lhs: NonTerminal, rdone: List[Symbol], todo: List[Symbol], look: Set[Terminal]) {
  def canReduce = todo.isEmpty
  def done = rdone.reverse
  def core = Core(lhs, rdone, todo)
  override def toString = lhs + " -> " + done.mkString(" ") + " . " + todo.mkString(" ") + look.mkString(" { ", " ", " }")
}

case class Core(lhs: NonTerminal, rdone: List[Symbol], todo: List[Symbol]) {
  def done = rdone.reverse
  override def toString = lhs + " -> " + done.mkString(" ") + " . " + todo.mkString(" ")
}

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

/**
 * A state of the LR(k) automaton.
 *
 *  Fields items, transitions, table are lazy to avoid excess computation for states that are not needed,
 *  and to defer computation until the state is complete and merged.
 */
class State(val kernel: mutable.Set[Item], val prev: State, val grammar: Grammar) {
  var number = 0
  val core = kernel map (_.core)
  val merged = mutable.Set[State]()

  def successors = {
    transitions map { case (_, next) => next }
  }

  def goto = {
    transitions collect { case (n: NonTerminal, next) => (n, next) }
  }

  def action = {
    val terminals = grammar.terminals + End
    val result = terminals map { t => (t, resolve(t, canShift(t), canReduce(t))) }
    result.toMap
  }

  /** Parse table for the shift/reduce parser  */
  lazy val table = {
    Table(action, goto)
  }

  /** Generate input sequences (including nonterminals) leading to this state */
  def paths(tails: Set[List[Symbol]], seen: Set[State]): Set[List[Symbol]] = {
    if (prev == null) {
      tails
    } else if (seen contains this) {
      Set()
    } else {
      val edges = prev.transitions collect {
        case (symbol, next) if next == this => symbol
      }
      val extensions = for (tail <- tails; symbol <- edges) yield {
        symbol :: tail
      }
      prev paths (extensions, seen + this)
    }
  }

  /** Merge two sets of actions, avoiding shift/reduce and reduce/reduce conflicts */
  def resolve(t: Terminal, shift: Iterable[Action], reduce: Iterable[Action]): Action = {
    // XX: consider precedence here
    assert(shift.size <= 1)
    assert(reduce.size <= 1)
    val maybeShift = shift.headOption
    val maybeReduce = reduce.headOption
    (maybeShift, maybeReduce) match {
      case (Some(shift), Some(reduce)) =>
        println("paths: ")
        for (path <- paths(Set(List()), Set()))
          println("  " + path.mkString(" "))
        println()
        println("merged: ")
        for (state <- merged)
          println(state.dump)
        sys.error("conflict in state " + number + " on " + t + " between " + shift + " and " + reduce)
      case (Some(shift), None) =>
        shift
      case (None, Some(reduce)) =>
        reduce
      case (None, None) =>
        Reject
    }
  }

  def canShift(t: Terminal) = items collect {
    case item @ Item(lhs, _, `t` :: _, _) =>
      Shift(transitions(t))
  }

  def canReduce(t: Terminal) = items collect {
    case item @ Item(lhs, rdone, Nil, look) if (look contains t) =>
      if (lhs == Start && t == End) {
        Accept
      } else {
        val rhs = rdone.reverse
        val List(rule) = grammar.rules collect {
          case rule @ Rule(`lhs`, `rhs`, _, _) =>
            rule
        }
        Reduce(rule)
      }
  }

  def canMerge(that: State): Boolean = {
    if (this.core != that.core)
      return false
    for (left <- this.items; right <- that.items if left != right) {
      if (left.canReduce && right.canReduce && !(left.look disjoint right.look))
        return false
    }
    return true
  }

  /** Item closure from the kernel set */
  lazy val items = {
    val result = mutable.Set[Item]()
    val todo = mutable.Queue[Item]()
    todo ++= kernel

    while (!todo.isEmpty) {
      val item = todo.dequeue
      if (!(result contains item)) {
        result += item

        item match {
          case Item(_, _, (lhs: NonTerminal) :: rest, look) =>
            // XXX: find a way not to recompute first for each iteration
            val follow = grammar.first(rest, look)
            for (Rule(`lhs`, rhs, _, _) <- grammar.rules) {
              val item = Item(lhs, Nil, rhs, follow)
              todo enqueue item
            }
          case _ =>
        }
      }
    }

    result
  }

  /** Successor states for each terminal/nonterminal */
  lazy val transitions = {
    val result = mutable.Map[Symbol, State]()

    val symbols = for (Item(_, _, shift :: rest, _) <- items) yield {
      shift
    }

    for (symbol <- symbols) {
      assert(!(result contains symbol))
      val kernel = for (item @ Item(lhs, rdone, `symbol` :: rest, look) <- items) yield {
        Item(lhs, symbol :: rdone, rest, look)
      }
      val next = new State(kernel, this, grammar)
      result += (symbol -> next)
    }

    result
  }

  override def toString = "state " + number

  def dump = {
    var res = this + "\n"
    res += "  previous: " + prev + "\n"
    for (item <- items) {
      res += "  " + item
      if (kernel contains item)
        res += " *"
      res += "\n"
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