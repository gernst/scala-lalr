package scalalr.parser

import scala.collection.mutable

import scalalr.util.Fixity
import scalalr.util.Left
import scalalr.util.Non
import scalalr.util.NonTerminal
import scalalr.util.Right
import scalalr.util.Symbol
import scalalr.util.Terminal

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

/**
 * A state of the LR(k) automaton.
 *
 *  Fields items, transitions, table are lazy to avoid excess computation for states that are not needed,
 *  and to defer computation until the state is complete and merged.
 */
class State(val kernel: mutable.Set[Item], val prev: State, val grammar: Grammar) {
  var number = 0
  val core = kernel map (_.core)
  val items = mutable.Set[Item]()
  val transitions = mutable.Map[Symbol, State]()
  val merged = mutable.Set[State]()
  
  def first = {
    items collect { case Item(_, _, shift :: rest, _) => shift }
  }
  
  def succ = {
    transitions map { case (_, next) => next }
  }

  def goto = {
    val result = transitions collect { case (n: NonTerminal, next) => (n, next) }
    result.toMap
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

  def conflict(t: Terminal, shift: Shift, reduce: Reduce): Nothing = {
    println("paths: ")
    for (path <- paths(Set(List()), Set()))
      println("  " + path.mkString(" "))
    println()
    println("merged: ")
    for (state <- merged)
      println(state.dump)
    sys.error("conflict in state " + number + " on " + t + " between " + shift + " and " + reduce)
  }

  /** Merge two sets of actions, avoiding shift/reduce and reduce/reduce conflicts */
  def resolve(t: Terminal, shift: Iterable[Action], reduce: Iterable[Action]): Action = {
    assert(shift.size <= 1)
    assert(reduce.size <= 1)
    val maybeShift = shift.headOption
    val maybeReduce = reduce.headOption

    (maybeShift, maybeReduce) match {
      case (Some(shift: Shift), Some(reduce: Reduce)) =>
        val which = Fixity.resolve(reduce.rule.prec, t.fixity)
        which match {
          case Left => reduce
          case Right => shift
          case Non => conflict(t, shift, reduce)
        }
      case (Some(shift), None) =>
        shift
      case (None, Some(reduce)) =>
        reduce
      case (None, None) =>
        Reject
      case _ =>
        sys.error("unexpected combinations of actions: " + maybeShift + " " + maybeReduce)
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
          case rule @ Rule(`lhs`, `rhs`, _, _, _) =>
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

  def merge(that: State): Boolean = {
    val changed = !(that.kernel subsetOf this.kernel)
    
    println("merging")
    println(this.dump)
    println(that.dump)
    if(changed) println("again " + this)

    this.kernel ++= that.kernel
    this.items ++= that.items

    this.merged ++= that.merged
    this.merged += that

    /* Fix transitions from state's predecessor */
    if (that.prev != null) {
      for ((symbol, next) <- that.prev.transitions if that == next) {
        that.prev.transitions(symbol) = this
      }
    }

    changed
  }
  
  def recompute() {
    computeItems()
    computeTransitions()
  }

  /** Item closure from the kernel set */
  def computeItems() {
    items.clear()
    val todo = mutable.Queue[Item]()
    todo ++= kernel

    while (!todo.isEmpty) {
      val item = todo.dequeue
      if (!(items contains item)) {
        items += item

        item match {
          case Item(_, _, (lhs: NonTerminal) :: rest, look) =>
            // XXX: find a way not to recompute first for each iteration
            val follow = grammar.first(rest, look)
            for (Rule(`lhs`, rhs, _, _, _) <- grammar.rules) {
              val item = Item(lhs, Nil, rhs, follow)
              todo enqueue item
            }
          case _ =>
        }
      }
    }
  }

  /** Successor states for each terminal/nonterminal */
  def computeTransitions() {
    transitions.clear()
    for (symbol <- first) {
      assert(!(transitions contains symbol))
      val kernel = for (item @ Item(lhs, rdone, `symbol` :: rest, look) <- items) yield {
        Item(lhs, symbol :: rdone, rest, look)
      }
      val next = new State(kernel, this, grammar)
      transitions += (symbol -> next)
    }
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
