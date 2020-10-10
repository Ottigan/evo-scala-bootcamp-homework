package homework.Sandbox

object Sandbox extends App {

  trait Printable extends Any {
    def print(): Unit = println(this)
  }
  class Wrapper(val underlying: Int) extends AnyVal with Printable

  val w = new Wrapper(3)
}
