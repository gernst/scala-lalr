package lalr.util

object Buffer {
  val minimalSize = 4
}

class Buffer {
  var buf = new Array[Char](Stack.minimalSize)

  var end = 0
  def length = end

  def grow() {
    val more = new Array[Char](buf.length * 2)
    System.arraycopy(buf, 0, more, 0, end)
    buf = more
  }

  def append(c: Char) {
    if (end == buf.length)
      grow()
    buf(end) = c
    end += 1
  }

  def append(i: Int) {
    assert(0 <= i && i <= Char.MaxValue)
    append(i.toChar)
  }

  def shift(n: Int) = {
    assert(n <= end)
    val res = new String(buf, 0, n)
    end -= n
    System.arraycopy(buf, n, buf, 0, end)
    res
  }

  override def toString = {
    (buf take end).mkString("Buffer(", ",", ")")
  }
}