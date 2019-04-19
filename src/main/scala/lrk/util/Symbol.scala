package lrk.util

sealed trait Symbol

trait Terminal extends Symbol {
  def fixity: Fixity
}

trait NonTerminal extends Symbol {
  // def ->(rhs: Symbol*) = Rule(this, rhs.toList, collapse)
}

sealed trait Assoc
case object Non extends Assoc
case object Left extends Assoc
case object Right extends Assoc

sealed trait Fixity {
  def prec: Int
  def assoc: Assoc
}

trait NonAssoc {
  self: Fixity =>
  def assoc = Non
}

object Fixity {
  val default = Nilfix(0)

  def resolve(reduce_prec: Int, shift: Fixity): Assoc = {
    if (reduce_prec > shift.prec)
      Left
    else if (shift.prec > reduce_prec)
      Right
    else
      shift.assoc
  }
}

case class Nilfix(prec: Int) extends Fixity with NonAssoc
case class Prefix(prec: Int) extends Fixity with NonAssoc
case class Postfix(prec: Int) extends Fixity with NonAssoc
case class Infix(prec: Int, assoc: Assoc) extends Fixity