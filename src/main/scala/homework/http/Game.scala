package homework.http

import cats.implicits.catsSyntaxOptionId

final case class Game(min: Int, max: Int, attempts: Int, numberToGuess: RandomNumber, id: String)
object Game {
  def create(t: Template, optionOfNumber: Option[RandomNumber], id: String): Option[Game] = optionOfNumber match {
    case Some(number) if t.min <= t.max && t.attempts > 0 => Game(t.min, t.max, t.attempts, number, id).some
    case None                                             => None
  }
}
