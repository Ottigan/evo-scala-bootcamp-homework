package homework.basics

import scala.io.Source

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
    def outcome: Double
    def result: Result
  }

  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command {
      def outcome: Double = dividend / divisor
      def result: Result = Result.Divide(dividend, divisor, outcome)
    }

    final case class Sum(numbers: List[Double]) extends Command {
      def outcome: Double = numbers.sum
      def result: Result = Result.Sum(numbers, outcome)
    }

    final case class Average(numbers: List[Double]) extends Command {
      def outcome: Double = numbers.sum / numbers.size
      def result: Result = Result.Average(numbers, outcome)
    }

    final case class Min(numbers: List[Double]) extends Command {
      def outcome: Double = numbers.min
      def result: Result = Result.Min(numbers, outcome)
    }

    final case class Max(numbers: List[Double]) extends Command {
      def outcome: Double = numbers.max
      def result: Result = Result.Max(numbers, outcome)
    }
  }

  final case class ErrorMessage(value: String) {
    val msg: String = s"Error: $value"
  }

  sealed trait Result {
    val prefix: String
    val numbers: List[Double]
    val outcome: Double
    def outcomeWithoutZeros: String = outcome.toString.replaceAll("[0]*$", "")
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

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    val list: List[String] = x.trim.replaceAll("\\s+", " ").split(" ").toList

    list match {
      case x :: y :: z :: Nil if x == "divide" => Right(Command.Divide(y.toDouble, z.toDouble))
      case x :: xs if x == "sum"               => Right(Command.Sum(xs.map(_.toDouble)))
      case x :: xs if x == "average"           => Right(Command.Average(xs.map(_.toDouble)))
      case x :: xs if x == "min"               => Right(Command.Min(xs.map(_.toDouble)))
      case x :: xs if x == "max"               => Right(Command.Max(xs.map(_.toDouble)))
      case _                                   => Left(ErrorMessage("Unsupported/Missing Command"))
    }

    // Implementation hints:
    // You can use String#split, convert to List using .toList, then pattern match on:
    //   case x :: xs => ???

    // Consider how to handle extra whitespace gracefully (without errors).
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    import Command._

    x match {
      case x: Divide  => Right(x.result)
      case x: Sum     => Right(x.result)
      case x: Average => Right(x.result)
      case x: Min     => Right(x.result)
      case x: Max     => Right(x.result)
      case _          => Left(ErrorMessage("Calculate Placeholder"))
    }
  }

  def renderResult(x: Result): String = {
    import Result._

    x match {
      case divide: Divide   => divide.result
      case sum: Sum         => sum.result
      case average: Average => average.result
      case min: Min         => min.result
      case max: Max         => max.result
    }
  }

  def process(x: String): String = {
    import cats.implicits._
    // the import above will enable useful operations on Either-s such as `leftMap`
    // (map over the Left channel) and `merge` (convert `Either[A, A]` into `A`),
    // but you can also avoid using them using pattern matching.

    val result = for {
      comm   <- parseCommand(x)
      result <- calculate(comm)
    } yield renderResult(result)

    result match {
      case Left(e)  => e.msg
      case Right(x) => x
    }
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines().map(process).foreach(println)
}
