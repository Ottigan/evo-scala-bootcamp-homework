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

  sealed trait Command

  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final case class ErrorMessage(value: String) {
    override def toString: String = s"Error: $value"
  }

  final case class Result(command: Command, result: Double)

  def parseCommand(line: String): Either[ErrorMessage, Command] = {
    import Command._

    // Solving extra whitespace
    val list: List[String] = line.trim.replaceAll("\\s+", " ").split(" ").toList

    val numbers: List[Either[String, Double]] = list.tail.map(x =>
      x.toDoubleOption match {
        case Some(y) => Right(y)
        case None    => Left(x)
      }
    )
    def leftNumbers: List[String] = numbers.collect {
      case Left(x) => x
    }
    def rightNumbers: List[Double] = numbers.collect {
      case Right(x) => x
    }

    val supportedCommands: List[String] = List("divide", "sum", "average", "min", "max")

    (list.headOption, numbers) match {
      case (Some(x), _) if !supportedCommands.contains(x.toLowerCase) =>
        Left(ErrorMessage("Missing/Unsupported Command"))
      case (_, y) if y.isEmpty                                        => Left(ErrorMessage("Numbers were not provided"))
      case (_, _) if leftNumbers.nonEmpty                             =>
        Left(ErrorMessage(s"Numbers required, incorrect format: ${leftNumbers.mkString(" ")}"))
      case (Some(x), _)                                               =>
        (x.toLowerCase, rightNumbers) match {
          case ("divide", y)  => y match {
              case x :: xs :: Nil => Right(Divide(x, xs))
              case _              => Left(ErrorMessage("2 numbers expected"))
            }
          case ("sum", y)     => Right(Sum(y))
          case ("average", y) => Right(Average(y))
          case ("min", y)     => Right(Min(y))
          case ("max", y)     => Right(Max(y))
        }
    }
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    import Command._

    x match {
      case Divide(dividend, divisor) =>
        if (divisor != 0) Right(Result(x, dividend / divisor))
        else Left(ErrorMessage("Division by zero is not allowed"))
      case Sum(numbers)              => Right(Result(x, numbers.sum))
      case Average(numbers)          => Right(Result(x, numbers.sum / numbers.length))
      case Min(numbers)              => Right(Result(x, numbers.min))
      case Max(numbers)              => Right(Result(x, numbers.max))
    }
  }

  def renderResult(x: Result): String = {
    import Command._

    val outcome = x.result

    // Removing unneeded zeros after the decimal
    def withoutTrailingZeros(number: Double): String = number.toString.replaceAll("[.]?[0]*$", "")

    def numbersToString(numbers: List[Double]): String = {
      numbers.map(withoutTrailingZeros).mkString(" ")
    }

    x.command match {
      case Divide(dividend, divisor) =>
        s"${withoutTrailingZeros(dividend)} divided by ${withoutTrailingZeros(divisor)} is ${withoutTrailingZeros(outcome)}"
      case Sum(x)                    => s"the sum of ${numbersToString(x)} is ${withoutTrailingZeros(outcome)}"
      case Average(x)                => s"the average of ${numbersToString(x)} is ${withoutTrailingZeros(outcome)}"
      case Min(x)                    => s"the min of ${numbersToString(x)} is ${withoutTrailingZeros(outcome)}"
      case Max(x)                    => s"the max of ${numbersToString(x)} is ${withoutTrailingZeros(outcome)}"
    }
  }

  def process(line: String): String = {
    val result = for {
      command <- parseCommand(line)
      result  <- calculate(command)
    } yield renderResult(result)

    result match {
      case Left(error)   => s"$error"
      case Right(result) => result
    }
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines().map(process).foreach(println)
}
