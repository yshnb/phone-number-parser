import scala.util.parsing.combinator._

object PhoneNumberParser extends RegexParsers {
  def phoneNumber = """\d{2,3}""".r ~ "-" ~ """\d{3,4}""".r ~ "-" ~ """\d{4}""".r ^^ {
    case (outerDigit ~ "-" ~ innerDigit ~ "-" ~ personalDigit) => PhoneNumber(outerDigit, innerDigit, personalDigit)
  }
  def apply(input: String): Either[String, PhoneNumber] = parseAll(phoneNumber, input) match {
    case Success(phoneNumberData, next) => Right(phoneNumberData)
    case NoSuccess(errorMessage, next) => Left(s"$errorMessage on line ${next.pos.line} on column ${next.pos.column}")
  }

  def main(args: Array[String]) {
    println(PhoneNumberParser("03-5907-6641"))
    println(PhoneNumberParser("090-3707-2326"))
    println(PhoneNumberParser("079-420-6027"))
    println(PhoneNumberParser("09037072326"))
  }
}
