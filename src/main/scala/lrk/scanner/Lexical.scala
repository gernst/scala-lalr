package lrk.scanner

import lrk.Regex
import lrk.util.Terminal

case class Rule(lhs: Terminal, rhs: Regex) {
  def item = Item(lhs, rhs)
}

case class Lexical(rules: List[Rule]) {
  
}