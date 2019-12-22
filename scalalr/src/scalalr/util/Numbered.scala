package scalalr.util

trait Numbering {
  context =>

  var index = 0
  def next = {
    index += 1
    index
  }

  trait numbered {
    val index = context.next
  }
}