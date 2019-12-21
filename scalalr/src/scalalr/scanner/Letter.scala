package scalalr.scanner

import scala.collection.immutable.BitSet

object Letter {
  def isPrintable(i: Letter) = {
    32 <= i && i < 128
    // Char.MinValue <= i && i <= Char.MaxValue && (i.toChar.isLetter || i.toChar.isDigit)
  }
  def fmt(i: Letter): String = {
    if (isPrintable(i)) i.toChar.toString
    else "\\u" + i.toString
  }
}

object Letters {
  val empty = BitSet.empty
  val alphabet = BitSet(0 to Char.MaxValue: _*)
  def single(c: Int) = BitSet(c)
  def range(first: Int, last: Int) = BitSet(first to last: _*)

  def compact(cs: Iterable[Letter]) = {
    val rs = runs(cs.toList.sorted.distinct)
    val ks = rs map {
      case (i, j) =>
        if (i == j) Letter.fmt(i) else Letter.fmt(i) + "-" + Letter.fmt(j)
    }
    ks mkString ("[", "", "]")
  }

  def run(c: Letter, cs: List[Letter]): Int = cs match {
    case `c` :: rest => 1 + run(c + 1, rest)
    case _ => 0
  }

  def runs(cs: List[Letter]): List[(Letter, Letter)] = if (cs.isEmpty) {
    return Nil
  } else {
    val i = run(cs.head, cs)
    assert(i > 0)
    (cs(0), cs(i - 1)) :: runs(cs drop i)
  }
}
