package lrk.test

import lrk._
import lrk.util._
import lrk.scanner.Letters

object test {
  sealed trait Expr
  case class Id(name: String) extends Expr
  case class Num(x: Int) extends Expr
  case class App(fun: String, args: List[Expr]) extends Expr

  object Unary extends ((String, Expr) => Expr) {
    def apply(fun: String, arg: Expr) = App(fun, List(arg))
  }

  object Binary extends ((Expr, String, Expr) => Expr) {
    def apply(arg1: Expr, fun: String, arg2: Expr) = App(fun, List(arg1, arg2))
  }

  object mode extends Mode {
    ignore(" +")
    val name = literal("[a-zäöü]+")

    val lparen = accept("\\(")
    val rparen = accept("\\)")
    val comma = accept(",")

    val plus = literal("+", Infix(1, Left))
    val minus = literal("-", Infix(1, Left))
    val star = literal("*", Infix(2, Left))
    // val uminus = literal("-", Prefix(3))

    val number = map("[0-9]+", _.toInt)
  }

  object grammar {
    import mode._
    val expr: Parser[Expr] = P(id | app | op | lit | parens)
    val exprs = expr ~* comma
    val id = Id(name)
    val lit = Num(number)
    val app = App(name ~ lparen ~ exprs ~ rparen)
    val op = Unary(minus ~ expr) | Binary(expr ~ minus ~ expr) | Binary(expr ~ plus ~ expr) | Binary(expr ~ star ~ expr)
    val parens = lparen ~ expr ~ rparen
  }

  def main(args: Array[String]) {
    val scanner = Scanner(mode)
    val parser = grammar.expr

    for (state <- parser.states) {
      // println(state.dump)
    }
    println("-------------------------------")
    val in = scanner.scan("0*(-a+-1)")
    val result = parser.parse(in)
    println(result)
  }
}