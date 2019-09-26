package lalr

import java.io.File
import java.io.FileReader
import java.io.Reader
import java.io.StringReader

import scala.collection.mutable

import lalr.scanner.DFA
import lalr.scanner.Letter
import lalr.scanner.Letters
import lalr.util.Fixity
import lalr.util.Generate
import lalr.util.Terminal

case class Token(symbol: Terminal, text: String, range: Range, position: Position)

object Tokens {
  def apply(tokens: (String, Terminal)*) = {
    var pos = 0
    for((text, symbol) <- tokens) yield {
      val range = Range(pos, text.length)
      val position = Position(0, pos)
      val token = Token(symbol, text, range, position)
      pos += text.length
      token
    }
  }
}

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
  def apply(pat: String): Regex = {

    def test(c: Char, i: Int): Int = {
      if (pat(i) == c) i + 1
      else throw new IllegalArgumentException("expected " + c + " at position " + i + " in " + pat)
    }

    def range(i0: Int): (Letters, Int) = pat(i0) match {
      case ']' =>
        (Letters.empty, i0)

      case start =>
        val i1 = test(start, i0)
        if (pat(i1) == '-') {
          val i2 = test('-', i1)
          val end = pat(i2)
          val i3 = test(end, i2)
          val ls0 = Letters.range(start, end)
          val (ls1, i4) = range(i3)
          (ls0 | ls1, i4)
        } else {
          val ls0 = Letters.single(start)
          val (ls1, i2) = range(i1)
          (ls0 | ls1, i2)
        }
    }

    def atom(i0: Int): (Regex, Int) = pat(i0) match {
      case '(' =>
        val i1 = test('(', i0)
        val (re, i2) = parse(i1)
        val i3 = test(')', i2)
        (re, i3)
      case '[' =>
        val i1 = test('[', i0)
        val (ls, i2) = range(i1)
        val i3 = test(']', i2)
        (Regex.letters(ls), i3)
      case ')' =>
        (???, test('.', i0))
      case ']' =>
        (???, test('.', i0))
      case '\\' =>
        val i1 = test('\\', i0)
        val c = pat(i1)
        val i2 = test(c, i1)
        (Regex.letters(c), i2)
      case c =>
        val i1 = test(c, i0)
        (Regex.letters(c), i1)
    }

    def post(re: Regex, i0: Int): (Regex, Int) = pat(i0) match {
      case '?' =>
        (re.?, test('?', i0))
      case '*' =>
        (re.*, test('*', i0))
      case '+' =>
        (re.+, test('+', i0))
    }

    def parse(i0: Int) = {
      val (re, i1) = atom(i0)
      if (i1 < pat.length)
        post(re, i1)
      else
        (re, i1)
    }

    val (re, n) = parse(0)
    assert(n == pat.length)
    re
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
    letters(Letters.single(b))
  }

  def seq(es: Iterable[Regex]): Regex = {
    es.foldRight(epsilon)(_ ~ _)
  }

  // def letters(s: String): Regex = {
  //   val bs = s.getBytes
  //   seq(bs.map(b => letters(b)))
  // }

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
  val whitespace = mutable.ListBuffer[Regex]()
  val regexps = mutable.ListBuffer[WithRegex]()

  def extend = {
    val mode = new Mode
    mode.regexps ++= this.regexps
    mode
  }

  def ignore(re: Regex) {
    if (!(whitespace contains re))
      whitespace += re
  }

  def ignore(pat: String) {
    ignore(Regex(pat))
  }

  def accept(re: Regex, fixity: Fixity): Recognizer = {
    val recognizer = Recognizer.regex(re, fixity)
    if (!(regexps contains recognizer))
      regexps += recognizer
    recognizer
  }

  def accept(pat: String, fixity: Fixity = Fixity.default): Recognizer = {
    accept(Regex(pat), fixity)
  }
  
  def literal(re: Regex, fixity: Fixity): Parser[String] = {
    val parser = Parser.regex(re, fixity)
    if (!(regexps contains parser))
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

  def compile(mode: Mode) {
    val (init, states) = DFA.states(DFA.translate(mode))
    mode.state = init
  }

  def scan(text: String): Iterator[Token] = {
    scan(new StringReader(text))
  }

  def scan(file: File): Iterator[Token] = {
    scan(new FileReader(file))
  }

  def scan(in: Reader): Iterator[Token] = {
    compile(init)
    for (mode <- other) compile(mode)
    DFA.scan(in, this)
  }

  def generate(dir: String, pkg: String, klass: String) {
    val name = pkg + "." + klass
    val out = Generate.open(dir, name)
    out.println("package " + pkg)
    out.println()
    out.println("class " + klass + " {")
    out.println("  var state: lalr.scanner.State = _")
    out.println("}")
    out.flush
    out.close
  }
}

