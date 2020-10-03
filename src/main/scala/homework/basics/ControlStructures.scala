package homework.basics

import scala.io.Source
import scala.util.{Failure, Success, Try}

object ControlStructures {
  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command {
    val outcome: Try[Double]
    def result: Result
  }

  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command {
      val outcome: Try[Double] = Try(
        // Casting to Int for expression to produce a Failure
        // Considering in case of Double it produces Success(Infinity)
        if (divisor == 0) dividend.toInt / divisor.toInt
        // Otherwise sticking with Double for decimal precision
        else dividend / divisor
      )
      def result: Result = Result.Divide(dividend, divisor, outcome.get)

    }

    final case class Sum(numbers: List[Double]) extends Command {
      val outcome: Try[Double] = Try(numbers.sum)
      def result: Result = Result.Sum(numbers, outcome.get)
    }

    final case class Average(numbers: List[Double]) extends Command {
      val outcome: Try[Double] = Try(numbers.sum / numbers.size)
      def result: Result = Result.Average(numbers, outcome.get)
    }

    final case class Min(numbers: List[Double]) extends Command {
      val outcome: Try[Double] = Try(numbers.min)
      def result: Result = Result.Min(numbers, outcome.get)
    }

    final case class Max(numbers: List[Double]) extends Command {
      val outcome: Try[Double] = Try(numbers.max)
      def result: Result = Result.Max(numbers, outcome.get)
    }
  }

  final case class ErrorMessage(value: String) {
    val msg: String = s"Error: $value"
  }

  sealed trait Result {
    val prefix: String
    val numbers: List[Double]
    val outcome: Double
    def outcomeWithoutZeros: String = outcome.toString.replaceAll("[.][0]*$", "")
    def inputToString: String = numbers
      .foldLeft("")((acc, x) => {
        if (x % 1 == 0) acc + s"${x.toInt} "
        else acc + s"$x "
      })
      .trim
    def result: String = s"$prefix $inputToString is $outcomeWithoutZeros"
  }

  object Result {
    final case class Divide(dividend: Double, divisor: Double, outcome: Double) extends Result {
      val prefix = " divided by "
      val numbers = List(dividend, divisor)
      override def result = f"${super.inputToString.replace(" ", prefix)} is ${super.outcomeWithoutZeros}"
    }

    final case class Sum(numbers: List[Double], outcome: Double) extends Result {
      val prefix = "the sum of"
    }

    final case class Average(numbers: List[Double], outcome: Double) extends Result {
      val prefix = "the average of"
    }

    final case class Min(numbers: List[Double], outcome: Double) extends Result {
      val prefix = "the minimum of"
    }

    final case class Max(numbers: List[Double], outcome: Double) extends Result {
      val prefix = "the maximum of"
    }
  }

  def parseCommand(line: String): Either[ErrorMessage, Command] = {
    import Command._
    // Solving extra whitespace
    val list: List[String] = line.trim.replaceAll("\\s+", " ").split(" ").toList

    // Could have written just .head
    // Considering Java#split, even on an empty String will always produce an Array("")
    // thus existence of "head" is guaranteed
    val command: String = list.headOption.getOrElse("none").toLowerCase

    val numbers: Try[List[Double]] = Try(list.tail.map(_.toDouble))

    numbers match {
      case Failure(e)                                    => Left(ErrorMessage(e.toString))
      case Success(x :: y :: Nil) if command == "divide" => Right(Divide(x, y))
      case Success(x) if x.nonEmpty                      =>
        command match {
          case "sum"     => Right(Sum(x))
          case "average" => Right(Average(x))
          case "min"     => Right(Min(x))
          case "max"     => Right(Max(x))
          case _         => Left(ErrorMessage("Incorrect Command or use of it"))
        }
      // Handling _.toDouble on an empty List producing Success(x) where x.isEmpty
      case _                                             => Left(ErrorMessage("Expression was too short"))
    }
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    // Checking if calculation was successful
    x.outcome match {
      case Success(_) => Right(x.result)
      case Failure(e) => Left(ErrorMessage(e.toString))
    }
  }

  def renderResult(x: Result): String = {

    x.result
  }

  def process(line: String): String = {
    import cats.implicits._
    // the import above will enable useful operations on Either-s such as `leftMap`
    // (map over the Left channel) and `merge` (convert `Either[A, A]` into `A`),
    // but you can also avoid using them using pattern matching.

    val result = for {
      command <- parseCommand(line)
      result  <- calculate(command)
    } yield renderResult(result)

    result match {
      case Left(error)   => error.msg
      case Right(result) => result
    }
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines().map(process).foreach(println)
}
