import scala.language.implicitConversions

package object lrk {
  def id[A](a: A): A = a

  implicit class SetOps[A](self: Set[A]) {
    def disjoint(that: Set[A]) = {
      (self & that).isEmpty
    }
  }

  def P[A](parser: => Parser[A])(implicit name: sourcecode.Name) = {
    Parser.named(name.value, () => parser)
  }

  implicit def toSequence0(p: Recognizer) = Sequence.of0(List(p))
  implicit def toSequence1[A](p: Parser[A]) = Sequence.of1(List(0), List(p))

  def $[A](a: A) = {
    Recognizer.empty map a
  }

  implicit class ListParser[A](ps: Parser[List[A]]) {
    def ::(p: Parser[A]) = (p ~ ps) map {
      (a: A, as: List[A]) => a :: as
    }
  }

  implicit class Constr0[Z](f: Z) {
    def apply(p: Sequence.of0): Parser[Z] = Parser.apply0(f, p.rparsers)
  }

  implicit class Constr1[A, Z](f: (A => Z)) {
    def apply(p: Sequence.of1[A]): Parser[Z] = Parser.apply1(f, p.rindex, p.rparsers)
  }

  implicit class Constr2[A, B, Z](f: (A, B) => Z) {
    def apply(p: Sequence.of2[A, B]): Parser[Z] = Parser.apply2(f, p.rindex, p.rparsers)
  }

  implicit class Constr3[A, B, C, Z](f: (A, B, C) => Z) {
    def apply(p: Sequence.of3[A, B, C]): Parser[Z] = Parser.apply3(f, p.rindex, p.rparsers)
  }
}