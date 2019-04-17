package lrk

package object internal {
  def id[A](a: A): A = a

  implicit class SetOps[A](self: Set[A]) {
    def disjoint(that: Set[A]) = {
      (self & that).isEmpty
    }
  }
}