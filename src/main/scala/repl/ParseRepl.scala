package repl.parse

import testing._

import parse._
import parse.base._

// What we'd like to have
object ParseRepl {
  //def char(c: Char): Parser[Char]
  //def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def testString():Either[ParseError, String] = {
    val p = BaseParsers
    p.run(p.string("de"))("def")
  }

  def testRegex(): Either[ParseError, String] = {
    val p = BaseParsers
    p.run(p.regex("[a-z]+".r))("def")
  }

  def testChar(): Either[ParseError, Int] = {
    val p = BaseParsers
    val charA = p.char('a')
    val numA = p.map(p.zeroOrMore(charA))(cs => cs.size)
    
    p.run(numA)("aaaa")
  }

  def testFlatMap(): Either[ParseError, String] = {
    val p = BaseParsers
    p.run(p.flatMap(p.string("d"))((a) =>  p.slice(p.unit(a))))("def")
  }

  /* EXERCISE 9.6 */
  // Create a context-sensitive parser that parses a number, then
  // uses that number to parse that many characters from an input

  def exercise9Dot6(): Either[ParseError, List[String]] = {
    val p = BaseParsers
    val parser =
    p.flatMap(p.regex("[\\d]+".r))((a) => {
      val charsToTake = a.toInt
      p.listOfN(charsToTake, p.regex("[a-z]".r))
    })

    p.run(parser)("4aaaa")
  }

  def testDigitsAndWhitespace(): Either[ParseError, String] = {
    val p = BaseParsers
    val parser =
      p.map2(
        p.map2(p.digits, p.whitespace)((d, w) => d),
        p.digits
      )((d,e) => d + e)

    p.run(parser)("123  456")
  }

  def testSkip(): Either[ParseError, String] = {
    val p = BaseParsers
    val p1 =
      p.skipLeft(p.char(','), p.digits)

    val p2 =
      p.skipRight(p.digits, p.whitespace)

    val parser = p.map2(p2, p1)((a, b) => a + ", " + b)

    p.run(parser)("123\n  ,456")
  }

  def test(): Boolean = {
    val p = BaseParsers
    val xParser = p.char('x')
    val whitespaceAndValue = p.map2(p.whitespace, xParser)((_,x) => x)
    val parser = p.or(whitespaceAndValue, xParser)

    p.run(parser)(" x") == p.run(parser)("x")
  }

  def testSep(): List[Either[ParseError,List[String]]] = {
    val p = BaseParsers
    val whitespaceOrDigits = p.map2(p.whitespace, p.digits)((_,d) => d)
    val zom = p.sep(whitespaceOrDigits, p.char(','))
    val oom = p.sep1(whitespaceOrDigits, p.char(','))

    List(
      p.run(oom)(" 123 456 789"),
      p.run(oom)(" 123, 456,789"),
      p.run(zom)(""),
      p.run(zom)("123"),
      p.run(zom)("123,456"))
  }
}



object TestCases {
  // run(or(string("abra"), string("cadabra")))("abra") == Right("abra")
  // run(or(string("abra"), string("cadabra")))("cadabra") == Right("cadabra")
  // run(listOfN(3, or(string("ab"), string("cad"))))("ababcad") == Right("ababcad")
  // run(listOfN(3, or(string("ab"), string("cad"))))("cadabab") == Right("cadabab")
  // run(listOfN(3, or(string("ab"), string("cad"))))("ababab") == Right("ababab")
  // run(numA)("aaa") == Right(3)
  // run(numA)("b") == Right(0)
}
