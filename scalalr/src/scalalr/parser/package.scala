package scalalr

package object parser {
  def id[A](a: A): A = a

  implicit class SetOps[A](self: Set[A]) {
    def disjoint(that: Set[A]) = {
      (self & that).isEmpty
    }
  }
}
