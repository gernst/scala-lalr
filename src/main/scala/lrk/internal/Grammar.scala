package lrk.internal

import scala.annotation.migration

sealed trait Symbol

trait Terminal extends Symbol {
}

trait NonTerminal extends Symbol {
  // def ->(rhs: Symbol*) = Rule(this, rhs.toList, collapse)
}

case object End extends Terminal
case object Start extends NonTerminal

case class Rule(lhs: NonTerminal, rhs: List[Symbol], rindex: List[Int], apply: Any) {
  def symbols = Set(lhs) ++ rhs
  def terminals = Set() ++ (rhs collect { case n: Terminal => n })
  def nonterminals = Set(lhs) ++ (rhs collect { case n: NonTerminal => n })
  override def toString = lhs + " -> " + rhs.mkString(" ")
}

object Rule {
  def singleton(lhs: NonTerminal, rhs: Symbol) = Rule(lhs, List(rhs), List(0), id[Any] _)
}

case class Grammar(start: NonTerminal, rules: List[Rule]) {
  val lhs = Set() ++ rules map (_.lhs)
  val symbols = Set() ++ (rules flatMap (_.symbols))
  val terminals = Set() ++ (rules flatMap (_.terminals))
  val nonterminals = Set() ++ (rules flatMap (_.nonterminals))

  /**
   * Nonterminals that can occur at the beginning of next,
   *  fall back to the old lookahead if next may be reduced to the empty string.
   *
   *  Used to compute the follow set for a potential reduction
   *  and thus to update the lookahead of items during closure.
   */
  def first(next: List[Symbol], look: Set[Terminal], seen: Set[NonTerminal]): Set[Terminal] = next match {
    case Nil =>
      look
    case (t: Terminal) :: _ =>
      Set(t)
    case (n: NonTerminal) :: _ if seen contains n =>
      Set()
    case (n: NonTerminal) :: rest =>
      val sets = for (Rule(`n`, rhs, _, _) <- rules) yield {
        first(rhs ++ next, look, seen + n)
      }
      Set(sets.flatten: _*)
  }

  def first(next: List[Symbol], look: Set[Terminal]): Set[Terminal] = {
    type X = (List[Symbol], Set[Terminal])
    type A = Terminal

    val first: X = (next, look)
    val states: Iterable[X] = List(first)

    def init(s: X): Iterable[A] = s match {
      case (Nil, look) => look
      case ((t: Terminal) :: _, _) => Set(t)
      case ((n: NonTerminal) :: _, _) => Set()
    }

    def succ(s: X): Iterable[X] = s match {
      case (Nil, look) => List()
      case ((t: Terminal) :: _, _) => List()
      case ((n: NonTerminal) :: rest, look) =>
        for (Rule(`n`, rhs, _, _) <- rules) yield {
          (rhs ++ rest, look)
        }
    }

    val result = Digraph.digraph[X, A](states, init, succ)
    Set() ++ result(first)
  }

  def dump: String = {
    var res = ""
    res += "start: " + start + "\n"
    for (rule <- rules) {
      res += "  " + rule + "\n"
    }
    res
  }
}