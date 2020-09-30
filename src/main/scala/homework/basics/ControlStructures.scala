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

  final case class ErrorMessage(value: String)

  sealed trait Result {
    def result: String
  }

  object Result {
    final case class Divide(dividend: Double, divisor: Double, outcome: Double) extends Result {
      def result: String = f"$dividend%.0f divided by $divisor%.0f is $outcome"
    }

    final case class Sum(numbers: List[Double], outcome: Double) extends Result {
      val prefix = "the sum of"
      val listToString: String = numbers.foldLeft("")((acc, x) => {
        if (x % 1 == 0) acc + s"${x.toInt} "
        else acc + s"$x "
      })
      def result: String = s"$prefix ${listToString}is $outcome"
    }

    final case class Average(numbers: List[Double], outcome: Double) extends Result {
      val prefix: String = "the average of"
      val listToString: String = numbers.foldLeft("")((acc, x) => {
        if (x % 1 == 0) acc + s"${x.toInt} "
        else acc + s"$x "
      })
      def result: String = s"$prefix ${listToString}is $outcome"
    }

    final case class Min(numbers: List[Double], outcome: Double) extends Result {
      val prefix: String = "the minimum of"
      val listToString: String = numbers.foldLeft("")((acc, x) => {
        if (x % 1 == 0) acc + s"${x.toInt} "
        else acc + s"$x "
      })
      def result: String = s"$prefix ${listToString}is $outcome"
    }

    final case class Max(numbers: List[Double], outcome: Double) extends Result {
      val prefix: String = "the maximum of"
      val listToString: String = numbers.foldLeft("")((acc, x) => {
        if (x % 1 == 0) acc + s"${x.toInt} "
        else acc + s"$x "
      })
      def result: String = s"$prefix ${listToString}is $outcome"
    }
  }

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    val list: Array[String] = x.split(" ")
    val command: String = list.head
    val numbers: List[Double] = list.toList.tail.map(_.toDouble)

    command match {
      case "divide"  => Right(Command.Divide(list(1).toDouble, list(2).toDouble))
      case "sum"     => Right(Command.Sum(numbers))
      case "average" => Right(Command.Average(numbers))
      case "min"     => Right(Command.Min(numbers))
      case "max"     => Right(Command.Max(numbers))
      case _         => Left(ErrorMessage("Error: Unsupported/Missing Command"))
    }

    // Implementation hints:
    // You can use String#split, convert to List using .toList, then pattern match on:
    //   case x :: xs => ???

    // Consider how to handle extra whitespace gracefully (without errors).
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    x match {
      case divide: Command.Divide   => Right(divide.result)
      case sum: Command.Sum         => Right(sum.result)
      case average: Command.Average => Right(average.result)
      case min: Command.Min         => Right(min.result)
      case max: Command.Max         => Right(max.result)
      case _                        => Left(ErrorMessage("Error: derp"))
    }
  }

  def renderResult(x: Result): String = {
    x match {
      case divide: Result.Divide   => divide.result
      case sum: Result.Sum         => sum.result
      case average: Result.Average => average.result
      case min: Result.Min         => min.result
      case max: Result.Max         => max.result
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
      case Left(e)  => e.value
      case Right(x) => x
    }
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines().map(process).foreach(println)
}
