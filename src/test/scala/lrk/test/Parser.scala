package lrk.test

import scala.language.postfixOps

import lrk._

object parser {
  sealed trait Expr
  case object X extends Expr
  case class Many(es: List[Expr]) extends Expr

  val lparen = Recognizer.literal("(")
  val rparen = Recognizer.literal(")")
  val x = Recognizer.literal("x")

  val expr: Parser[Expr] = P(many | X(x))
  val many = Many(lparen ~ (expr *) ~ rparen)

  def main(args: Array[String]) {
    val grammar = LR.translate(expr)
    val (init, states) = LR.states(grammar)
    println()

    val sorted = states.toList.sortBy(_.number)

    println(grammar.dump)
    println()

    for (state <- sorted) {
      println(state.dump)
    }

    val inputs = Seq(
      Seq(Token(x, "x")),
      Seq(Token(lparen, "("), Token(x, "x"), Token(rparen, ")")))

    for (input <- inputs) {
      val result = LR.parse(input, init)
      println("input:  " + input.mkString(" "))
      println("result: " + result)
      println()
    }
  }
}