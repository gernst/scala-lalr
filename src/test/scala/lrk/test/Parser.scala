package lrk.test

import scala.language.postfixOps

import lrk._
import lrk.internal.LR

object parser {
  sealed trait Expr
  case object X extends Expr
  case object Y extends Expr
  case class Many(es: List[Expr]) extends Expr

  val lparen = Recognizer.literal("(")
  val rparen = Recognizer.literal(")")
  val x = Recognizer.literal("x")
  val y = Recognizer.literal("y")

  val expr: Parser[Expr] = P(many | X(x) | Y(y))
  val many = Many(lparen ~ (expr *) ~ rparen)

  def debug() {
    val grammar = LR.translate(expr)
    val (init, states) = LR.states(grammar)
    println()

    val sorted = states.toList.sortBy(_.number)

    println(grammar.dump)
    println()

    for (state <- sorted) {
      println(state.dump)
    }
  }

  def main(args: Array[String]) {
    val inputs = Seq(
      Seq(Token(x, "x", Range(0, 1))),
      Seq(Token(lparen, "(", Range(0, 1)), Token(x, "x", Range(1, 1)), Token(y, "y", Range(2, 1)), Token(rparen, ")", Range(3, 1))))

    for (input <- inputs) {
      val result = expr.parse(input)
      println("input:  " + input.mkString(" "))
      println("result: " + result)
      println()
    }
  }
}