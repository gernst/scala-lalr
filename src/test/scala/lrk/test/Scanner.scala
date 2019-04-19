package lrk.test

import lrk._
import java.io.StringReader
import lrk.util.Fixity

object scanner {
  def main(args: Array[String]) {
    object mode extends Mode
    val u = mode.accept(Regex.letters('u'), Fixity.default)
    val x = mode.literal(Regex.letters('x'), Fixity.default)
    val y = mode.value(Regex.letters('y'), 1, Fixity.default)
    val z = mode.map(Regex.letters('z'), _.length, Fixity.default)

    val scanner = Scanner(mode)
    val in = new StringReader("uxyz")
    val tokens = scanner scan in
    println(tokens.toList)
  }
}