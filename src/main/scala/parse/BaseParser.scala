package parse.base

import parse._
import parse.base._

import ReferenceTypes._

object ReferenceTypes {
  //type ParserType[+A] = String => Result[A]
  type ParserType[+A] = ParseState => Result[A]
  case class ParseState(loc: Location) {
    def advanceBy(numChars: Int): ParseState =
      copy(loc = loc.copy(offset = loc.offset + numChars))
    def input: String = loc.input.substring(loc.offset)
    def slice(n: Int) = loc.input.substring(loc.offset, loc.offset + n)
  }

  trait Result[+A] {
    /** For success, make sure to advance the offset */
    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a,m) => Success(a,n+m)
      case _ => this
    }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]
}


object BaseParsers extends Parsers[ParserType]{
  //type Parser[+A] = String => Result[A]

  // NOTE: This was a first pass that the book introduced
  /*def string(s: String): Parser[A] =
    (input: String) => {
      if (input.startsWith(s))
        Right(s)
      else
        Left(Location(input).toError("Expected: " + s))
    }*/

  /* EXERCISE 9.13 */
  // Implement string, regex, succeed(unit), and slice
  def unit[A](a: A): ParserType[A] =
    (parseState: ParseState) => Success(a, 0)

  def flatMap[A,B](pa: ParserType[A])(f: A => ParserType[B]): ParserType[B] =
    (parseState: ParseState) => pa(parseState) match {
      case Success(a, n) => {
        f(a)(parseState.advanceBy(n)).advanceSuccess(n)
      }
      case f@Failure(_) => f
    }

  def string(s: String): ParserType[String] =
    (parseState: ParseState) => {
      if (parseState.input.startsWith(s))
        Success(s, s.length)
      else
        Failure(parseState.loc.toError("Expected: " + s))
    }

  def or[A](p1: ParserType[A], p2: => ParserType[A]): ParserType[A] =
    (parseState: ParseState) => {
      p1(parseState) match {
        case Failure(e) => p2(parseState)
        case r => r
      }
    }

  def slice[A](p: ParserType[A]): ParserType[String] =
    (parseState: ParseState) => {
      p(parseState) match {
        case Success(a, n) => Success(parseState.slice(n), n)
        case f@Failure(_) => f
      }
    }

  def regex(r: scala.util.matching.Regex): ParserType[String] =
    (parseState: ParseState) => {
      r.findPrefixOf(parseState.input) match {
        case Some(m) => Success(m, m.length)
        case _ => Failure(parseState.loc.toError("Regex failed: " + r.toString))
      }
    }

  def run[A](p: ParserType[A])(input: String): Either[ParseError, A] =
    p(ParseState(Location(input))) match {
      case Success(a, n) => Right(a)
      case Failure(e) => Left(e)
    }

  override def zeroOrMore[A](p: ParserType[A]): ParserType[List[A]] =
    (parseState: ParseState) => {
      val buf = new collection.mutable.ListBuffer[A]
      def go(p: ParserType[A], offset: Int): Result[List[A]] = {
        p(parseState.advanceBy(offset)) match {
          case Success(a,n) => buf += a; go(p, offset+n)
          case Failure(e) => Success(buf.toList, offset)
        }
      }
      go(p, 0)
    }
}
