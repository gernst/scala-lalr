package scalalr

import scala.collection.BitSet

package object scanner {
  type Letter = Int
  type Letters = BitSet
  
  sealed trait Error extends Exception
  case class UnexpectedChar(next: Char, position: Position) extends Error
  case class UnexpectedEof(position: Position) extends Error
}
