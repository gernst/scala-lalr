package lrk.util

sealed trait Symbol

trait Terminal extends Symbol {
}

trait NonTerminal extends Symbol {
  // def ->(rhs: Symbol*) = Rule(this, rhs.toList, collapse)
}