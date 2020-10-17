package homework.error_handling

import java.time.LocalDate
import cats.data.ValidatedNec
import cats.implicits.catsSyntaxValidatedIdBinCompat0
import cats.syntax.all._

import scala.util.Try

object Homework extends App {

  // Homework. Place the solution under `error_handling` package in your homework repository.
  //
  // 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
  // 2. Add `ValidationError` cases (at least 5, may be more).
  // 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.
  type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

  sealed trait ValidationError
  object ValidationError {
    final case object UnsupportedNetwork extends ValidationError {
      override def toString: String = "Unsupported payment card network!"
    }
    final case object CardNumberIsNotNumeric extends ValidationError {
      override def toString: String = "Payment card number: Only digits 0-9 allowed!"
    }
    final case object PaymentCardNumberWrongPrefix extends ValidationError {
      override def toString: String = "Provided card number is not valid!"
    }
    final case object CardNumberTooLong extends ValidationError {
      override def toString: String = "Payment card number too long!"
    }
    final case object CardNumberTooShort extends ValidationError {
      override def toString: String = "Payment card number too short!"
    }
    final case object CardNameError extends ValidationError {
      override def toString: String = "Please provide a valid Name!"
    }
    final case object ExpirationDateInvalid extends ValidationError {
      override def toString: String = "Invalid date!"
    }
    final case object ExpiredCard extends ValidationError {
      override def toString: String = "Card has expired and is no longer valid!"
    }
    final case object SecurityCodeError extends ValidationError {
      override def toString: String = "Invalid security code!"
    }
  }

  sealed trait Network
  object Network {
    final case object Mastercard extends Network
    final case object Visa extends Network

    def create(network: String): AllErrorsOr[Network] = {
      if (network.toUpperCase == "VISA") Visa.validNec
      else if (network.toUpperCase == "MASTERCARD") Mastercard.validNec
      else ValidationError.UnsupportedNetwork.invalidNec
    }
  }

  final case class PaymentCardNumber(number: String)
  object PaymentCardNumber {
    def create(number: String, company: String): AllErrorsOr[PaymentCardNumber] = {
      import ValidationError._

      // Treating the provided String as if there are no spaces

      def numberNumeric: AllErrorsOr[PaymentCardNumber] = {
        if (!number.forall(x => Try(x.toInt).isSuccess)) CardNumberIsNotNumeric.invalidNec
        else PaymentCardNumber(number).validNec
      }
      def numberLength: AllErrorsOr[PaymentCardNumber] = {
        if (number.length > 16) CardNumberTooLong.invalidNec
        else if (number.length < 16) CardNumberTooShort.invalidNec
        else PaymentCardNumber(number).validNec
      }
      def numberPrefix: AllErrorsOr[PaymentCardNumber] = {
        if (company.toUpperCase == "VISA" && number.matches("^[4]\\d+")) PaymentCardNumber(number).validNec
        else if (company.toUpperCase == "MASTERCARD" && number.matches("^[5][1-5]\\d+"))
          PaymentCardNumber(number).validNec
        else ValidationError.PaymentCardNumberWrongPrefix.invalidNec
      }

      numberNumeric.productR(numberLength).productR(numberPrefix)
    }
  }

  case class Name(name: String) extends AnyVal
  object Name {
    def create(name: String): AllErrorsOr[Name] = {
      if (name.matches("^([A-Za-z]){3,}\\s([A-Za-z]){3,}$") && name.length < 22) Name(name).validNec
      else ValidationError.CardNameError.invalidNec
    }
  }

  case class ExpirationDate(date: String) extends AnyVal
  object ExpirationDate {
    def create(date: String): AllErrorsOr[ExpirationDate] = {
      val serverNow = LocalDate.now()

      def parseDate: AllErrorsOr[LocalDate] = {
        val dateWithoutSymbols = date.filter(_.isDigit)
        if (dateWithoutSymbols.length == 4) {
          val month = dateWithoutSymbols.slice(0, 2).toInt
          val year = dateWithoutSymbols.slice(2, 4).toInt
          if (month == 12) LocalDate.parse(s"20${year + 1}-01-01").validNec
          else LocalDate.parse(s"20$year-$month-01").validNec

          LocalDate.parse(s"20$year-$month-01").validNec
        } else ValidationError.ExpirationDateInvalid.invalidNec
      }

      def serverNowIsBefore(expirationDate: LocalDate): AllErrorsOr[ExpirationDate] = {
        if (serverNow.isBefore(expirationDate)) ExpirationDate(date).validNec
        else ValidationError.ExpiredCard.invalidNec
      }

      parseDate.andThen(serverNowIsBefore)
    }
  }

  case class SecurityCode(securityCode: String) extends AnyVal
  object SecurityCode {
    def create(securityCode: String): AllErrorsOr[SecurityCode] = {
      if (
        securityCode.forall(x => Try(x.toInt).isSuccess) &&
        securityCode.length == 3
      ) SecurityCode(securityCode).validNec
      else ValidationError.SecurityCodeError.invalidNec
    }
  }

  case class PaymentCard(
      company: Network,
      number: PaymentCardNumber,
      name: Name,
      expirationDate: ExpirationDate,
      securityCode: SecurityCode
  )

  object PaymentCardValidator {

    def validate(
        company: String,
        name: String,
        number: String,
        expirationDate: String,
        securityCode: String
    ): AllErrorsOr[PaymentCard] =
      (
        Network.create(company),
        PaymentCardNumber.create(number, company),
        Name.create(name),
        ExpirationDate.create(expirationDate),
        SecurityCode.create(securityCode)
      ).mapN(PaymentCard)
  }
}
