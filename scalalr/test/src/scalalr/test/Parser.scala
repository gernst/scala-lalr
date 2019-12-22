package scalalr.test

import scala.language.postfixOps

import scalalr._
import scalalr.parser.LALR

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
    val grammar = LALR.translate(expr)
    val (init, states) = LALR.states(grammar)
    println()

    println(grammar.dump)
    println()

    val sorted = states.toList.sortBy(_.index)
    for (state <- sorted) {
      println(state.dump)
    }
  }

  def main(args: Array[String]) {
    debug()

    /* val inputs = Seq(
      Tokens("x" -> x),
      Tokens("(" -> lparen, "x" -> x, "y" -> y, ")" -> rparen))

    for (input <- inputs) {
      val result = expr.parse(input)
      println("input:  " + input.mkString(" "))
      println("result: " + result)
      println()
    } */
  }
}
