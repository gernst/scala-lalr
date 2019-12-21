package scalalr.test

import scalalr._

object SmtLib2 {
  sealed trait Expr
  case class Id(name: String) extends Expr
  case class App(fun: Id, args: List[Expr]) extends Expr

  sealed trait Cmd

  object mode extends Mode {
    ignore(" +")

    val ident: Parser[String] = ???

    val lparen = accept("\\(")
    val rparen = accept("\\)")
  }

  object expr {
    import mode._
    import scala.language.postfixOps

    val expr: Parser[Expr] = P(id | app)
    val exprs = expr *

    val id = Id(ident)
    val app = App(lparen ~ id ~ exprs ~ rparen)
  }

  object cmd {
    import mode._
    import expr._
    import scala.language.postfixOps

    val cmd: Parser[Cmd] = P(???)

    val script = cmd *
  }
}
