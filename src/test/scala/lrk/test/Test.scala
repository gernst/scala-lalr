package lrk.test

import lrk._
import lrk.util._
import lrk.scanner.Letters

object test {
  sealed trait Expr
  case class Id(name: String) extends Expr
  case class App(fun: String, args: List[Expr]) extends Expr

  object mode extends Mode {
    ignore(" +")
    val name = literal("[a-z]+")
    val lparen = accept("\\(")
    val rparen = accept("\\)")
    val comma = accept(",")
  }

  object grammar {
    import mode._
    val expr: Parser[Expr] = P(id | app)
    val exprs = expr ~* comma
    val id = Id(name)
    val app = App(name ~ lparen ~ exprs ~ rparen)
  }

  def main(args: Array[String]) {
    val scanner = Scanner(mode)
    val parser = grammar.expr
    val in = scanner.scan("f(x)")
    val result = parser.parse(in)
    println(result)
  }
}