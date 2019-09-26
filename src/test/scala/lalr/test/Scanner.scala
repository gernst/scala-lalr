package lalr.test

import lalr._
import java.io.StringReader
import lalr.util.Fixity

object scanner {
  def main(args: Array[String]) {
    object mode extends Mode
    val u = mode.accept(Regex.letters('u'), Fixity.default)
    val x = mode.literal(Regex.letters('x'), Fixity.default)
    val y = mode.value(Regex.letters('y'), 1, Fixity.default)
    val z = mode.map(Regex.letters('z'), _.length, Fixity.default)

    val scanner = Scanner(mode)
    val tokens = scanner scan "uxyz"
    println(tokens.toList)
    
    scanner.generate("src/generated/scala", "lalr.test", "Scanner")
  }
}