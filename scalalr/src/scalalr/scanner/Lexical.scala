package scalalr.scanner

import scalalr.Regex
import scalalr.util.Terminal
import scalalr.util.Fixity

case object Whitespace extends Terminal {
  def fixity = Fixity.default
}

case class Rule(lhs: Terminal, rhs: Regex) {
  def item = Item(lhs, rhs)
}

case class Lexical(rules: List[Rule]) {
  
}
