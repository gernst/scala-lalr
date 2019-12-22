package scalalr

package object parser {
  import scalalr.util.Terminal

  sealed trait Error extends Exception
  case class UnexpectedSymbol(symbol: Terminal, position: Position) extends Error
  
  object timer {
    import util.Timer
    
    object items extends Timer("items")
    object transitions extends Timer("transitions")
    object merge extends Timer("merge")
  }
}
