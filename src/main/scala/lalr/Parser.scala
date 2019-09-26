package lalr

import lalr.parser.LALR
import lalr.parser.Rule
import lalr.util.NonTerminal
import lalr.util.Symbol
import lalr.util.Terminal
import lalr.util.Fixity

sealed trait Parseable {
  def normalize: List[List[Atomic]]
}

sealed trait Recognizer extends Parseable {
  def |(that: Recognizer): Recognizer = {
    Recognizer.choice(this, that)
  }
}

sealed trait Parser[+A] extends Parseable {
  def |[B >: A](that: Parser[B]): Parser[B] = {
    Parser.choice(this, that)
  }

  def *(): Parser[List[A]] = {
    Parser.rec("(" + this + ")*", (ps: Parser[List[A]]) => $(Nil) | this :: ps)
  }

  def +(): Parser[List[A]] = {
    this :: this.*
  }

  def ~*(sep: Recognizer): Parser[List[A]] = {
    this :: (sep ~ this).*
  }

  def ~+(sep: Recognizer): Parser[List[A]] = {
    this :: (sep ~ this).+
  }

  def reduceLeft[B >: A](f: (B, A) => B): Parser[B] = {
    this.+ map { (as: List[A]) => as.reduceLeft(f) }
  }

  def reduceRight[B >: A](f: (A, B) => B): Parser[B] = {
    this.+ map { (as: List[A]) => as.reduceRight(f) }
  }

  def foldLeft[B](z: => Parser[B])(f: (B, A) => B): Parser[B] = {
    (z ~ this.*) map { (b: B, as: List[A]) => as.foldLeft(b)(f) }
  }

  def foldRight[B](z: => Parser[B])(f: (A, B) => B): Parser[B] = {
    (this.* ~ z) map { (b: B, as: List[A]) => as.foldRight(b)(f) }
  }

  lazy val (init, states) = {
    LALR.states(LALR.translate(this))
  }

  def parse(in: Iterable[Token]): A = {
    parse(in.iterator)
  }

  def parse(in: Iterator[Token]): A = {
    LALR.parse(in, init, false).asInstanceOf[A]
  }

  def parseTree(in: Iterable[Token]): Node = {
    parseTree(in.iterator)
  }

  def parseTree(in: Iterator[Token]): Node = {
    LALR.parse(in, init, true).asInstanceOf[Node]
  }
}

sealed trait Atomic extends Parseable {
  def symbol: Symbol
  def normalize = List(List(this))
}

sealed trait WithRegex extends Atomic with Terminal {
  def symbol = this
  def re: Regex
}

sealed trait WithRules extends Atomic with NonTerminal {
  def symbol = this
  def expand: List[List[Atomic]]
  def apply: Any
  def collapse: Boolean
  def rindex: List[Int]

  def other: List[WithRules] = {
    expand flatMap (_ collect { case p: WithRules => p })
  }

  def rules: List[Rule] = {
    expand map (rhs => Rule(symbol, rhs map (_.symbol), rindex, apply, collapse))
  }
}

sealed trait Apply extends WithRules {
  def rparsers: List[Parseable]
  def parsers = rparsers.reverse

  def expand: List[List[Atomic]] = {
    parsers.foldRight(Recognizer.empty.normalize)(seq)
  }

  def seq(p: Parseable, Q: List[List[Atomic]]): List[List[Atomic]] = {
    for (
      ps <- p.normalize;
      qs <- Q
    ) yield ps ++ qs
  }

  override def toString = {
    parsers.mkString("(", " ~ ", ")@")
  }
}

object Recognizer {
  case object empty extends Recognizer {
    def normalize = List(Nil: List[Atomic])
    override def toString = "_"
  }

  case class apply0[Z](apply: () => Z, rparsers: List[Parseable], collapse: Boolean) extends Recognizer with Apply { def rindex = Nil }

  case class regex(re: Regex, fixity: Fixity) extends Recognizer with WithRegex {
    override def toString = "'" + re + "'"
  }

  case class literal(name: String) extends Recognizer with Atomic with Terminal {
    def symbol = this
    def fixity = Fixity.default
    override def toString = "'" + name + "'"
  }

  case class choice(left: Recognizer, right: Recognizer) extends Recognizer {
    def normalize = left.normalize ++ right.normalize
    override def toString = "(" + left + " | " + right + ")"
  }
}

object Parser {
  case class named[+A](name: String, parser: () => Parser[A]) extends Parser[A] with WithRules {
    lazy val self = parser()
    def expand = self.normalize

    def apply = (a: Any) => a
    def rindex = List(0)
    def collapse = true

    override def toString = name
  }

  case class rec[A](name: String, parser: Parser[A] => Parser[A]) extends Parser[A] with WithRules {
    lazy val self = parser(this)
    def expand = self.normalize

    def apply = (a: Any) => a
    def rindex = List(0)
    def collapse = true

    override def toString = name
  }

  case class regex(re: Regex, fixity: Fixity) extends Parser[String] with WithRegex {
    override def toString = "'" + re + "'"
  }

  case class choice[+A](left: Parser[A], right: Parser[A]) extends Parser[A] {
    def normalize = left.normalize ++ right.normalize
    override def toString = "(" + left + " | " + right + ")"
  }

  case class apply0[Z](apply: () => Z, rparsers: List[Parseable], collapse: Boolean) extends Parser[Z] with Apply { def rindex = Nil }
  case class apply1[A, Z](apply: (A) => Z, rindex: List[Int], rparsers: List[Parseable], collapse: Boolean) extends Parser[Z] with Apply
  case class apply2[A, B, Z](apply: (A, B) => Z, rindex: List[Int], rparsers: List[Parseable], collapse: Boolean) extends Parser[Z] with Apply
  case class apply3[A, B, C, Z](apply: (A, B, C) => Z, rindex: List[Int], rparsers: List[Parseable], collapse: Boolean) extends Parser[Z] with Apply
}

object Sequence {
  case class of0(rparsers: List[Parseable]) {
    def ~(p: Recognizer) = of0(p :: rparsers)
    def ~[A](p: Parser[A]) = of1[A](List(rparsers.length), p :: rparsers)
    def map[Z](f: => Z): Parser[Z] = Parser.apply0(() => f, rparsers, false)
    def flatMap[Z](f: => Z): Parser[Z] = Parser.apply0(() => f, rparsers, true)
  }

  case class of1[+A](rindex: List[Int], rparsers: List[Parseable]) extends Parser[A] with Apply {
    assert(rindex.length == 1)
    def apply = lalr.parser.id[Any] _
    def collapse = true
    def ~(p: Recognizer) = of1[A](rindex, p :: rparsers)
    def ~[B](p: Parser[B]) = of2[A, B](rparsers.length :: rindex, p :: rparsers)
    def map[A, Z](f: (A => Z)): Parser[Z] = Parser.apply1(f, rindex, rparsers, false)
    def flatMap[A, Z](f: (A => Z)): Parser[Z] = Parser.apply1(f, rindex, rparsers, true)
  }

  case class of2[+A, +B](rindex: List[Int], rparsers: List[Parseable]) {
    assert(rindex.length == 2)
    def ~(p: Recognizer) = of2[A, B](rindex, p :: rparsers)
    def ~[C](p: Parser[C]) = of3[A, B, C](rparsers.length :: rindex, p :: rparsers)
    def map[A, B, Z](f: (A, B) => Z): Parser[Z] = Parser.apply2(f, rindex, rparsers, false)
    def flatMap[A, B, Z](f: (A, B) => Z): Parser[Z] = Parser.apply2(f, rindex, rparsers, true)
  }

  case class of3[+A, +B, +C](rindex: List[Int], rparsers: List[Parseable]) {
    assert(rindex.length == 3)
    def ~[C](p: Recognizer) = of3[A, B, C](rindex, p :: rparsers)
    // XXX: more sequences
    def map[A, B, C, Z](f: (A, B, C) => Z): Parser[Z] = Parser.apply3(f, rindex, rparsers, false)
    def flatMap[A, B, C, Z](f: (A, B, C) => Z): Parser[Z] = Parser.apply3(f, rindex, rparsers, true)
  }
}