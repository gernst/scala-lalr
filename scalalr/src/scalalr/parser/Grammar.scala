package scalalr.parser

import scalalr.util.Digraph
import scalalr.util.Fixity
import scalalr.util.NonTerminal
import scalalr.util.Symbol
import scalalr.util.Terminal
import scalalr.util.id

case object End extends Terminal {
  def fixity = Fixity.default
}

case object Start extends NonTerminal

case class Rule(lhs: NonTerminal, rhs: List[Symbol], rindex: List[Int], apply: Any, collapse: Boolean) {
  def symbols = Set(lhs) ++ rhs
  def terminals = Set(rhs collect { case n: Terminal => n }: _*)
  def nonterminals = Set(lhs) ++ (rhs collect { case n: NonTerminal => n })
  def item = Item(lhs, Nil, rhs)

  def prec: Int = { // yuck code, where is .findLast?
    val n = rhs.lastIndexWhere(_.isInstanceOf[Terminal])
    if (n < 0) 0
    else rhs(n).asInstanceOf[Terminal].fixity.prec
  }

  override def toString = lhs + " -> " + rhs.mkString(" ")
}

object Rule {
  def singleton(lhs: NonTerminal, rhs: Symbol) = Rule(lhs, List(rhs), List(0), id[Any] _, false)
}

case class Grammar(start: NonTerminal, rules: List[Rule]) {
  val lhs = Set(rules map (_.lhs): _*)
  val symbols = Set(rules flatMap (_.symbols): _*)
  val terminals = Set(rules flatMap (_.terminals): _*)
  val nonterminals = Set(rules flatMap (_.nonterminals): _*)
  val index = rules groupBy (_.lhs)

  /**
   * Nonterminals that can occur at the beginning of next,
   *  fall back to the old lookahead if next may be reduced to the empty string.
   *
   *  Used to compute the follow set for a potential reduction
   *  and thus to update the lookahead of items during closure.
   */
  /* def first(next: List[Symbol], look: Set[Terminal], seen: Set[NonTerminal]): Set[Terminal] = next match {
    case Nil =>
      look
    case (t: Terminal) :: _ =>
      Set(t)
    case (n: NonTerminal) :: _ if seen contains n =>
      Set()
    case (n: NonTerminal) :: rest =>
      val sets = for (Rule(`n`, rhs, _, _, _) <- rules) yield {
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
        for (Rule(`n`, rhs, _, _, _) <- rules) yield {
          (rhs ++ rest, look)
        }
    }

    val result = Digraph.digraph[X, A](states, init, succ)
    Set() ++ result(first)
  } */

  def dump: String = {
    var res = ""
    res += "start: " + start + "\n"
    for (rule <- rules) {
      res += "  " + rule + "\n"
    }
    res
  }
}
