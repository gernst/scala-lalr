package scalalr.scanner

import java.io.Reader

import scala.collection.mutable
import scala.util.control.Breaks.break
import scala.util.control.Breaks.breakable

import scalalr.Mode
import scalalr.Position
import scalalr.Range
import scalalr.Scanner
import scalalr.Token
import scalalr.util.Buffer

object DFA {
  def translate(mode: Mode): Lexical = {
    val rules = new mutable.ListBuffer[Rule]()

    for (re <- mode.regexps)
      rules += Rule(re.symbol, re.re)
    for (re <- mode.whitespace)
      rules += Rule(Whitespace, re)

    Lexical(rules.toList)
  }

  def states(lexical: Lexical): (State, Seq[State]) = {
    var number = 0

    val todo = mutable.Queue[State]()
    val states = mutable.Buffer[State]()

    val init = new State(lexical.rules map (_.item), null)

    todo enqueue init

    while (!todo.isEmpty) {
      val state = todo.dequeue

      val that = states find (_.items == state.items)

      that match {
        case Some(that) =>
          if (state.prev != null) {
            for ((symbol, `state`) <- state.prev.transitions) {
              state.prev.transitions(symbol) = that
            }
          }

        case None =>
          number += 1
          state.number = number
          states += state
          todo ++= state.successors
      }
    }

    (init, states)
  }

  /**
   * Scan longest prefix of cs from a state.
   */
  def scan(in: Reader, scanner: Scanner) = {
    scanWithWhitespace(in, scanner) filterNot (_.symbol == Whitespace)
  }

  def scanWithWhitespace(in: Reader, scanner: Scanner) = new Iterator[Token] {
    val buf = new Array[Char](1)
    val result = new Buffer()

    var start = 0
    var letter: Char = _
    var atEof: Boolean = _

    var line = 0
    var column = 0

    step()

    def step() {
      atEof = (in.read(buf) < 0)
      letter = buf(0)

      if (letter == '\n') {
        line += 1
        column = 0
      } else {
        column += 1
      }
    }

    def hasNext = {
      !atEof
    }

    def next = {
      val init = scanner.state
      var state = init
      var accepting: Option[(State, Int)] = None

      breakable {
        while (hasNext) {
          assert(state != null)

          if (state.transitions contains letter) {
            result append letter
            state = state.transitions(letter)
            if (state.accepts) {
              accepting = Some((state, result.length))
            }
          } else {
            break
          }

          step()
        }
      }

      accepting match {
        case Some((state, length)) =>
          val text = result shift length
          val symbols = state.canAccept
          val range = Range(start, length)
          val position = Position(line, column)
          start += length
          Token(symbols.head, text, range, position)
        case None if hasNext =>
          sys.error("unexpected character " + Letter.fmt(letter))
        case None =>
          sys.error("unexpected end of input")
      }
    }
  }
}
