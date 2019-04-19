package lrk

import java.io.Reader

import scala.collection.mutable

import lrk.scanner.DFA
import lrk.scanner.Letter
import lrk.scanner.Letters
import lrk.util.Fixity
import lrk.util.Terminal

case class Token(symbol: Terminal, text: String, range: Range)

sealed trait Regex {
  import Regex._

  def first: Letters
  def isNullable: Boolean
  def derive(c: Letter): Regex

  def ?(): Regex = this | epsilon
  def |(that: Regex): Regex = or(Set(this, that))
  def &(that: Regex): Regex = and(Set(this, that))

  def unary_~(): Regex = this match {
    case not(e) => e
    case letters(cs) => letters(Letters.alphabet -- cs)
    case _ => not(this)
  }

  def ~(that: Regex): Regex = (this, that) match {
    case (`empty`, _) => empty
    case (_, `empty`) => empty
    case (`epsilon`, _) => that
    case (_, `epsilon`) => this
    case (seq(first, second), third) => seq(first, seq(second, third))
    case _ => seq(this, that)
  }

  def +(): Regex = {
    this ~ this.*
  }

  def *(): Regex = this match {
    case `empty` | `epsilon` => epsilon
    case rep(that) => that
    case _ => rep(this)
  }
}

object Regex {
  def apply(re: String): Regex = {
    ???
  }

  val epsilon: Regex = and(Set())
  val empty: Regex = or(Set())

  case class letters(first: Letters) extends Regex {
    def isNullable = {
      false
    }

    def derive(c: Letter) = if (first contains c) {
      epsilon
    } else {
      empty
    }

    override def toString = if (first.size == 1 && Letter.isPrintable(first.head)) {
      first.head.toChar.toString
    } else {
      Letters.compact(first)
    }
  }

  case class seq(e1: Regex, e2: Regex) extends Regex {
    def first = {
      if (e1.isNullable) e1.first | e2.first else e1.first
    }

    def isNullable = {
      e1.isNullable && e2.isNullable
    }

    def derive(c: Letter) = if (e1.isNullable) {
      ((e1 derive c) ~ e2) | (e2 derive c)
    } else {
      ((e1 derive c) ~ e2)
    }

    override def toString = {
      e1 + "" + e2
    }
  }

  case class rep(e: Regex) extends Regex {
    def first = {
      e.first
    }

    def isNullable = {
      true
    }
    def derive(c: Letter) = {
      (e derive c) ~ this
    }

    override def toString = e match {
      case _: seq => "(" + e + ")*"
      case _ => e + "*"
    }
  }
  case class not(e: Regex) extends Regex {
    def first = {
      Letters.alphabet -- e.first
    }

    def isNullable = {
      e.isNullable
    }

    def derive(c: Letter) = {
      ~(e derive c)
    }

    override def toString = e match {
      case `empty` => "."
      case _ => "~(" + e.toString + ")"
    }
  }
  case class any(es: Set[Regex]) extends Regex {
    def first = {
      es.foldLeft(Letters.empty)(_ | _.first)
    }

    def isNullable = {
      es exists (_.isNullable)
    }

    def derive(c: Letter) = {
      or(es map (_ derive c))
    }

    override def toString = {
      if (es.isEmpty) "_" else es mkString ("(", " | ", ")")
    }
  }

  case class all(es: Set[Regex]) extends Regex {
    def first = {
      es.foldLeft(Letters.empty)(_ & _.first)
    }

    def isNullable = {
      es forall (_.isNullable)
    }

    def derive(c: Letter) = {
      if (es.isEmpty) empty else and(es map (_ derive c))
    }

    override def toString = {
      if (es.isEmpty) "{}" else es mkString ("(", " & ", ")")
    }
  }

  def letters(b: Int): Regex = {
    assert(0 <= b && b <= Char.MaxValue)
    letters(Letters.empty + b)
  }

  def seq(es: Iterable[Regex]): Regex = {
    es.foldRight(epsilon)(_ ~ _)
  }

  def letters(s: String): Regex = {
    val bs = s.getBytes
    seq(bs.map(b => letters(b)))
  }

  def ors(e: Regex): Set[Regex] = e match {
    case any(es) => es flatMap ors
    case _ => Set(e)
  }

  def ands(e: Regex): Set[Regex] = e match {
    case all(es) => es flatMap ands
    case _ => Set(e)
  }

  def or(es: Set[Regex]) = es flatMap ors match {
    case Set1(e) => e
    case rs => any(rs)
  }

  def and(es: Set[Regex]) = es flatMap ands match {
    case Set1(e) => e
    case rs => all(rs)
  }
}

class Mode {
  var state: scanner.State = _
  val regexps = mutable.ListBuffer[WithRegex]()

  def extend = {
    val mode = new Mode
    mode.regexps ++= this.regexps
    mode
  }

  def accept(re: Regex, fixity: Fixity): Recognizer = {
    val recognizer = Recognizer.regex(re, fixity)
    regexps += recognizer
    recognizer
  }

  def accept(pat: String, fixity: Fixity = Fixity.default): Recognizer = {
    accept(Regex(pat), fixity)
  }

  def literal(re: Regex, fixity: Fixity): Parser[String] = {
    val parser = Parser.regex(re, fixity)
    regexps += parser
    parser
  }

  def literal(pat: String, fixity: Fixity = Fixity.default): Parser[String] = {
    literal(Regex(pat), fixity)
  }

  def value[A](re: Regex, a: A, fixity: Fixity): Parser[A] = {
    accept(re, fixity) map a
  }

  def value[A](pat: String, a: A, fixity: Fixity = Fixity.default): Parser[A] = {
    value(Regex(pat), a, fixity)
  }

  def map[A](re: Regex, f: String => A, fixity: Fixity): Parser[A] = {
    literal(re, fixity) map f
  }

  def map[A](pat: String, f: String => A, fixity: Fixity = Fixity.default): Parser[A] = {
    map(Regex(pat), f, fixity)
  }
}

case class Scanner(init: Mode, other: Mode*) {
  var mode: Mode = init
  def state = mode.state

  def scan(in: Reader): Iterator[Token] = {
    val (init, states) = DFA.states(DFA.translate(this))
    for (state <- states) {
      println(state.dump)
      println()
    }
    DFA.scan(in, this)
  }
}

