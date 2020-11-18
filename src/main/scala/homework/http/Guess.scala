package homework.http

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxFlatMapOps
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.client.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.Client
import org.http4s.circe.CirceEntityCodec._
import io.circe.generic.auto._
import scala.concurrent.ExecutionContext

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
final case class Template(min: Int, max: Int, attempts: Int)

object GuessServer extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 3001, host = "localhost")
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)

  var currentGames: GameCache = GameCache.create(Nil)

  private val httpApp = HttpRoutes.of[IO] {

    // curl -POST -v "localhost:3001/game" -d '{"min": 0, "max": 5, "attempts": 3}' -H "Content-Type: application/json"
    case req @ POST -> Root / "game"                          =>
      req.as[Template]
        .flatMap(template => {
          val randomNumber = RandomNumber.create(template.min, template.max)
          val gameID = java.util.UUID.randomUUID.toString
          val formattedGame = Game.create(template, randomNumber, gameID)

          formattedGame match {
            case Some(game) =>
              currentGames = currentGames.addGame(game)
              Ok(
                s"""Game has started! 
                   |You may begin guessing between ${template.min} and ${template.max} (Inclusive)""".stripMargin
              )
                .map(_.addCookie("gameID", gameID))
            case None       =>
              Ok(
                s"""Game could not be initiated from the provided parameters! 
                   |Min:${template.min} 
                   |Max:${template.max} 
                   |Attempts:${template.attempts}""".stripMargin
              )
          }
        })

    // curl -v "localhost:3001/game/guess/0" -b "gameID=#ID_PROVIDED_BY_INITIAL_POST_RESPONSE#"
    case req @ GET -> Root / "game" / "guess" / IntVar(guess) =>
      val gameID = req.cookies.find(_.name == "gameID")
      val gameIDValue = gameID.flatMap(id => Option(id.content))

      gameIDValue match {
        case Some(id) => currentGames.games.find(_.id == id) match {
            case Some(game) =>
              if (guess == game.numberToGuess.num) {
                currentGames = currentGames.removeGame(game)
                Ok(s"Congratulations $guess was correct!").map(_.removeCookie("gameID"))
              } else if (game.attempts == 1) {
                if (guess > game.numberToGuess.num) {
                  currentGames = currentGames.removeGame(game)
                  Ok(s"Your guess of $guess was too high, you lose!").map(_.removeCookie("gameID"))
                } else {
                  currentGames = currentGames.removeGame(game)
                  Ok(s"Your guess of $guess was too low, you lose!").map(_.removeCookie("gameID"))
                }
              } else {
                if (guess > game.numberToGuess.num) {
                  currentGames = currentGames.updateGame(game)
                  Ok(s"Your guess of $guess was too high! Attempts left: ${game.attempts - 1}").map(_.addCookie(
                    "gameID",
                    id
                  ))
                } else {
                  currentGames = currentGames.updateGame(game)
                  Ok(s"Your guess of $guess was too low! Attempts left: ${game.attempts - 1}").map(_.addCookie(
                    "gameID",
                    id
                  ))
                }
              }
            case None       => BadRequest("Don't cheat the system!")
          }
        case None     => BadRequest("You are an alien!")
      }
  }.orNotFound
}

object GuessClient extends IOApp {
  import org.http4s.Method._

  private val uri = uri"http://localhost:3001"

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  private def letTheGameBegin(client: Client[IO], response: Response[IO]): IO[Unit] = for {
    _       <- response.as[String] >>= printLine
    _       <- printLine()
    cookies <- IO(response.cookies)
    _       <- cookies.find(_.name == "gameID") match {
      case Some(v) if v.content.nonEmpty /* #removeCookie removes only the content */ =>
        for {
          req <- GET(uri / "game" / "guess" / "4").map(_.addCookie(RequestCookie(v.name, v.content)))
          _   <- printLine(string = "Trying to guess the number:")
          res <- client.run(req).use(IO(_))
          _   <- letTheGameBegin(client, res)
        } yield ()
      case _                                                                          => IO(())
    }
  } yield ()

  def run(args: List[String]): IO[ExitCode] =
    BlazeClientBuilder[IO](ExecutionContext.global).resource.use(client =>
      for {
        _              <- printLine(string = "Providing game parameters:")
        initialRequest <- POST(Template(0, 5, 3), uri / "game")
        firstResponse  <- client.run(initialRequest).use(IO(_))
        _              <- letTheGameBegin(client, firstResponse)
      } yield ()
    ).as(ExitCode.Success)
}
