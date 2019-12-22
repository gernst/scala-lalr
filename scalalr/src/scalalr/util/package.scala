package scalalr

package object util {
  def id[A](a: A): A = a

  def time[A](msg: String)(a: => A) = {
    val start = System.currentTimeMillis
    val res = a
    val end = System.currentTimeMillis
    println("time '" + msg + "': " + (end - start) + "ms")
    res
  }

  implicit class SetOps[A](self: Set[A]) {
    def disjoint(that: Set[A]) = {
      (self & that).isEmpty
    }
  }

  object Set1 {
    def unapply[A](s: Set[A]) = {
      if (s.size == 1) Some(s.head)
      else None
    }
  }
}