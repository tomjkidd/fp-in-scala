package parse

import language.higherKinds

import testing._

trait Parsers[Parser[+_]] {
  /** Create a successful parser for a given A */
  def unit[A](a: A): Parser[A]

  def map[A, B](pa: Parser[A])(f: A => B): Parser[B] =
    mapUsingFlatMap(pa)(f)

  def map2[A,B,C](pa: Parser[A], pb: => Parser[B])(f: (A, B) => C): Parser[C] =
    map2UsingFlatMap(pa, pb)(f)
  
  /* EXERCISE 9.1 */

  /*def map2UsingProduct[A,B,C](pa: Parser[A], pb: Parser[B])(f: (A, B) => C): Parser[C] =
    product(pa, pb)(f.tupled)*/

  def flatMap[A,B](pa: Parser[A])(f: A => Parser[B]): Parser[B]

  /** Recognize a single character */
  def char(c: Char): Parser[Char] =
    map(string(c.toString))(s => s.charAt(0))

  /** Recognize a string */
  def string(s: String): Parser[String]

  /** Recognize one of two options */
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  /** Recognize repetition */
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    listOfNUsingMap2AndUnit(n, p)

  def slice[A](p: Parser[A]): Parser[String]

  def many[A](p: Parser[A]): Parser[List[A]] = zeroOrMore(p)

  def many1[A](p: Parser[A]): Parser[List[A]] = oneOrMore(p)

  def run[A](p: Parser[A])(input: String): Either[ParseError,A]


  /* Discovery */
  
  /* EXERCISE 9.3 */

  def zeroOrMore[A](p: Parser[A]): Parser[List[A]] =
    // define many in terms of or, map2, and unit
    or(map2(p, zeroOrMore(p))((a, b) => a :: b), unit(List()))

  def oneOrMore[A](p: Parser[A]): Parser[List[A]] =
    map2(p, zeroOrMore(p))((a, b) => a :: b)

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    productUsingFlatMap(p, p2)

  /* EXERCISE 9.4 */

  def listOfNUsingMap2AndUnit[A](n: Int, pa: Parser[A]): Parser[List[A]] =
    if(n <= 0)
      unit(Nil)
    else
      map2(pa, listOfNUsingMap2AndUnit(n-1, pa))((a, b) => a :: b)

  /* EXERCISE 9.5 */
  // TODO: Deal with the non-strictness of Parser[B] with a separate combinator
  // like in Chapter 7. Make the necessary changes to the api. What do you think
  // of this approach?

  def regex(r: scala.util.matching.Regex): Parser[String]

  /* EXERCISE 9.7 */
  // Implement product and map2 in terms of flatMap
  def productUsingFlatMap[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    flatMap(p)((a) => flatMap(p2)((b) => unit((a,b))))

  def map2UsingFlatMap[A,B,C](pa: Parser[A], pb: => Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(pa)((a) => map(pb)((b) => f(a,b)))

  /* EXERCISE 9.8 */
  // Express map in terms of flatMap
  def mapUsingFlatMap[A, B](pa: Parser[A])(f: A => B): Parser[B] =
    flatMap(pa)((a) => unit(f(a)))

  /* Error handling combinators */

  /** Assign an error message to a parser */
  /*def label[A](msg: String)(p: Parser[A]): Parser[A]
  def errorLocation(e: ParseError): Location
  def errorMessage(e: ParseError): String
  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]*/

  /* EXERCISE 9.10 */

  // TODO: Determine a nice set of combinators for expressing what errors get reported
  // by a parser. For each combinator, try to come up with a set of laws.

  /* EXERCISE 9.11 */
  // TODO: Any other primitives that might be useful for letting the programmer
  // specify what error(s) in an 'or' chain get reported?

  /* EXERCISE 9.12 */
  // TODO: Use the primitive combinators to create an implementation
  // {string, regex, slice, label, scope, flatMap, attempt, or}

  /* EXERCISE 9.13 */
  // TODO: Implement string, regex, succeed, and slice for the reference implementation

  /* EXERCISE 9.14 */
  // TODO: Revise string to use scope and/or label to provide meaningful error msgs

  /* EXERCISE 9.15 */
  // TODO: Implement the rest of the primitives, including run, using this
  // representation of a parser, try running on JSON Parser

  /* EXERCISE 9.16 */
  // TODO: Come up with a nice way of formatting a ParseError. One possibility
  // is to combine or group labels by location.

  /* EXERCISE 9.17 */
  // TODO: The slice combinator is still less efficient than it could be. For
  // instance, many(char('a')).slice will still build up a List[Char], only to
  // discard it. Can you think of a way of modifying the Parser representation
  // to make slicing more efficient?

  /* EXERCISE 9.18 */
  // TODO: Some info is lost when we combine using or. If both parsers fail,
  // we're only keeping the errors from the second parser. We might want to
  // show both, or whichever got further. Change ParseError to keep track of errors
  // that occured in other branches of the parser.

  object Laws {
    // run(char(c))(c.toString) == Right(c)
    // run(string(s))(s) == Right(s)
    // map(p)(a => a) == a
    // run(unit(a))(s) == Right(a)

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, map(p)(a => a))(in)

    /* EXERCISE 9.2 */
    // TODO: Try coming up with laws to specify the behavior of product
    /*def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
      Prop.forAll(inputs ** Gen.string) { case (input: String, msg: String) =>
        run(label(msg)(p))(input) match {
          case Left(e: ParseError) => errorMessage(e) == msg
          case _ => true
        }
      }*/
  }
}

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))
}

case class ParseError(stack: List[(Location, String)]) {
  
}

