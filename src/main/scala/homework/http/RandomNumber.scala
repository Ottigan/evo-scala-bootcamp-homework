package homework.http

import cats.implicits.catsSyntaxOptionId
import scala.util.{Random, Try}

case class RandomNumber(num: Int) extends AnyVal
object RandomNumber {
  def create(min: Int, max: Int): Option[RandomNumber] = Try(Random.between(min, max + 1)).toOption match {
    case Some(v) => RandomNumber(v).some
    case None    => None
  }
}
