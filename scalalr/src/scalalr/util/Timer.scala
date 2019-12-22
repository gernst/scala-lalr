package scalalr.util

class Timer(name: String) {
  var time = 0L

  def apply[A](a: => A) = {
    val start = System.currentTimeMillis
    val res = a
    val end = System.currentTimeMillis
    time += end - start
    res
  }

  override def toString = {
    name + ": " + time + "ms"
  }
}