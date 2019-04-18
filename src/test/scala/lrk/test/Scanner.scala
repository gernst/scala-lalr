package lrk.test

import lrk._
import java.io.StringReader

object scanner {
  def main(args: Array[String]) {
    object mode extends Mode
    val u = mode.accept(Regex.letters('u'))
    val x = mode.literal(Regex.letters('x'))
    val y = mode.value(Regex.letters('y'), 1)
    val z = mode.map(Regex.letters('z'), _.length)

    val scanner = Scanner(mode)
    val in = new StringReader("uxyz")
    val tokens = scanner scan in
    println(tokens.toList)
  }
}