package scalalr.util

import scala.reflect.ClassTag

object Stack {
  val minimalSize = 4
}

class Stack[A: ClassTag]() extends (Int => A) {
  var stack = new Array[A](Stack.minimalSize)

  var end = 0
  def length = end

  def grow() {
    val more = new Array[A](stack.length * 2)
    System.arraycopy(stack, 0, more, 0, end)
    stack = more
  }

  def top = {
    stack(end - 1)
  }

  def push(a: A) {
    if (end == stack.length)
      grow()
    stack(end) = a
    end += 1
  }

  def pop(): A = {
    end -= 1
    stack(end)
  }

  def drop(n: Int) {
    end -= n
  }

  def apply(index: Int) = {
    stack(end - index - 1)
  }
  
  override def toString = {
    (stack take end).mkString("Stack(", ",", ")")
  }
}