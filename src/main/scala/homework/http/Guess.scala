package homework.http

import cats.effect.{ExitCode, IO, IOApp}
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.client.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.circe.CirceEntityCodec._

import scala.concurrent.ExecutionContext
import io.circe.generic.auto._

import scala.util.Random

// Homework. Place the solution under `http` package in your homework repository.
//
// Write a server and a client that play a number guessing game together.
//
// Communication flow should be as follows:
// 1. The client asks the server to start a new game by providing the minimum and the maximum number that can
//    be guessed, as well as the maximum number of attempts.
// 2. The server comes up with some random number within the provided range.
// 3. The client starts guessing the number. Upon each attempt, the server evaluates the guess and responds to
//    the client, whether the current number is lower, greater or equal to the guessed one.
// 4. The game ends when the number is guessed or there are no more attempts left. At this point the client
//    should terminate, while the server may continue running forever.
// 5. The server should support playing many separate games (with different clients) at the same time.
//
// Use HTTP or WebSocket for communication. The exact protocol and message format to use is not specified and
// should be designed while working on the task.
final case class Game(min: Int, max: Int, attempts: Int)

object GuessServer extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 3001, host = "localhost")
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)

  private val httpApp = HttpRoutes.of[IO] {

    // curl -POST -v "localhost:3001/game" -d '{"min": 0, "max": 5, "attempts": 3}' -H "Content-Type: application/json"
    case req @ POST -> Root / "game"                          =>
      req.as[Game]
        .flatMap(game => {
          val randomNumber = Random.between(game.min, game.max)
          Ok(s"Game has started, you may begin guessing between ${game.min} and ${game.max}")
            .map(_
              .addCookie("attemptsLeft", game.attempts.toString)
              .addCookie("numberToGuess", randomNumber.toString))
        })

    // curl -v "localhost:3001/game/guess/2" -b "numberToGuess=3;attemptsLeft=3"
    case req @ GET -> Root / "game" / "guess" / IntVar(guess) =>
      val numberToGuessCookie = req.cookies.find(_.name == "numberToGuess")
      val numberToGuessValue = numberToGuessCookie.flatMap(_.content.toIntOption)
      val attemptsLeftCookie = req.cookies.find(_.name == "attemptsLeft")
      val attemptsLeftValue = attemptsLeftCookie.flatMap(_.content.toIntOption).map(_ - 1)

      (numberToGuessValue, attemptsLeftValue) match {
        case (Some(numberToGuess), Some(remainingAttempts)) if remainingAttempts >= 0 =>
          if (numberToGuess == guess) {
            Ok(s"Congratulations $guess was correct!")
          } else if (remainingAttempts > 0) {
            if (guess > numberToGuess) {
              Ok(s"Your guess was too high! $remainingAttempts attempts remain!")
                .map(_
                  .addCookie("numberToGuess", numberToGuess.toString)
                  .addCookie("attemptsLeft", remainingAttempts.toString))
            } else {
              Ok(s"Your guess was too low! $remainingAttempts attempts remain!")
                .map(_
                  .addCookie("numberToGuess", numberToGuess.toString)
                  .addCookie("attemptsLeft", remainingAttempts.toString))
            }
          } else {
            if (guess > numberToGuess) {
              Ok(s"Your guess was too high! You lose!")

            } else {
              Ok(s"Your guess was too low! You lose!")
            }
          }
        case (_, _)                                                                   => Ok("Spoiled cookies... eww!")
      }
  }.orNotFound
}

object GuessClient extends IOApp {
  import org.http4s.Method._

  private val uri = uri"http://localhost:3001"

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  def run(args: List[String]): IO[ExitCode] =
    BlazeClientBuilder[IO](ExecutionContext.global).resource.use(client =>
      for {
        _ <- printLine(string = "Providing game parameters:")
        _ <- client.expect[String](POST(Game(0, 5, 3), uri / "game"))
          .flatMap(printLine)
        _ <- printLine()

        _ <- printLine(string = "Trying to guess the number:")
        _ <- client.expect[String](GET(uri / "game" / "guess" / "4")
          .map(_
            .addCookie("numberToGuess", "3")
            .addCookie("attemptsLeft", "3")))
          .flatMap(printLine)
      } yield ()
    ).as(ExitCode.Success)
}
