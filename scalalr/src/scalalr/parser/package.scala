package scalalr

package object parser {
  import scalalr.util.Terminal

  def id[A](a: A): A = a

  implicit class SetOps[A](self: Set[A]) {
    def disjoint(that: Set[A]) = {
      (self & that).isEmpty
    }
  }

  sealed trait Error extends Exception
  case class UnexpectedSymbol(symbol: Terminal, position: Position) extends Error
}
