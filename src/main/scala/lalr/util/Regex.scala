package lalr.util

import lalr._
import lalr.scanner.Letters

object regex {
  trait Common {
    this: Mode =>
    def delim(c: Char, fixity: Fixity = Fixity.default) = accept(Regex.letters(Letters.single(c)), fixity)
    def token(c: Char, fixity: Fixity = Fixity.default) = literal(Regex.letters(Letters.single(c)), fixity)
  }

  val char = Regex.letters(Letters.alphabet)

  object mode extends Mode with Common {
    val lparen = delim('(')
    val rparen = delim(')')

    val lbrack = delim('[')
    val rbrack = delim(']')
    val pipe = delim('|')
    val amp = delim('&')
    val tilde = delim('~')
    val star = delim('*')
    val plus = delim('+')

    val to = delim('-')
    val dot = delim('.')
    val back = delim('\\')

    val alpha = literal(char, Fixity.default)
  }

  object grammar {
    import mode._

    val re: Parser[Regex] = P(any | lit | seq | not | or | and | rep | rep1)

    val any = char(dot)
    val lit = alpha map Regex.seq
    val seq = re.* map Regex.seq
    val rep = re ~ star map ((_: Regex).*)
    val rep1 = re ~ plus map ((_: Regex).+)
    val not = tilde ~ re map (~(_: Regex))
    val or = re ~ pipe ~ re map ((_: Regex) | (_: Regex))
    val and = re ~ amp ~ re map ((_: Regex) | (_: Regex))

    val escaped = back ~
      ('('(lparen) |
        ')'(rparen) |
        '['(lbrack) |
        ']'(rbrack) |
        '|'(pipe) |
        '&'(amp) |
        '~'(tilde) |
        '*'(star) |
        '+'(plus) |
        '-'(to) |
        '.'(dot))

    // val range =
    // val klass = lbrack ~ range.* ~ rbrack
  }
  
  def main(args: Array[String]) {
    mode
    grammar.re.init
  }
}