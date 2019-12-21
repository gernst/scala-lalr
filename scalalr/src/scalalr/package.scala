import scala.language.implicitConversions

package object scalalr {
  def S[A](init: Mode, other: Mode*) = {
    Scanner(init, other: _*)
  }

  def P[A](parser: => Parser[A])(implicit name: sourcecode.Name) = {
    Parser.named(name.value, () => parser)
  }

  implicit def toSequence0(p: Recognizer) = Sequence.of0(List(p))
  implicit def toSequence1[A](p: Parser[A]) = Sequence.of1[A](List(0), List(p))

  def $[A](a: A) = {
    Recognizer.empty map a
  }

  object Set1 {
    def unapply[A](s: Set[A]) = {
      if (s.size == 1) Some(s.head)
      else None
    }
  }

  implicit class ListParser[A](ps: Parser[List[A]]) {
    def ::(p: Parser[A]) = (p ~ ps) flatMap {
      (a: A, as: List[A]) => a :: as
    }
  }

  implicit class StringRegexConversion(pat: String) {
    def re = Regex(pat)
  }

  implicit class Constr0[Z](f: Z) {
    def apply(p: Recognizer): Parser[Z] = p map f
    def apply(p: Sequence.of0): Parser[Z] = p map f
  }

  implicit class Constr1[A, Z](f: (A => Z)) {
    def apply(p: Parser[A]): Parser[Z] = p map f
    def apply(p: Sequence.of1[A]): Parser[Z] = p map f
  }

  implicit class Constr2[A, B, Z](f: (A, B) => Z) {
    def apply(p: Sequence.of2[A, B]): Parser[Z] = p map f
  }

  implicit class Constr3[A, B, C, Z](f: (A, B, C) => Z) {
    def apply(p: Sequence.of3[A, B, C]): Parser[Z] = p map f
  }
}
