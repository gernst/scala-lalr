package lrk

import lrk.internal.Terminal

case class Token(symbol: Terminal, text: String, range: Range)

trait Scanner {
  
}