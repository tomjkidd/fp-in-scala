package parse.json

trait JSON

object JSON {
  import parse._
  import parse.base.ReferenceTypes._

  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  /** A simple JSON parser, that doesn't allow whitespace everywhere.
    This is meant to just show parsers in source code in their simplest form */
  def simpleJsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {

    def jnull: Parser[JSON] = {
      P.map(P.string("null"))(_ => JNull)
    }

    def jtrue: Parser[JSON] = {
      P.map(P.string("true"))(_ => JBool(true))
    }

    def jfalse: Parser[JSON] = {
      P.map(P.string("false"))(_ => JBool(false))
    }

    def jstring: Parser[JSON] = {
      P.map(P.quoted)(str => JString(str))
    }

    def jnumber: Parser[JSON] = {
      val jsonNumber = P.regex("-?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][+-]?\\d+)?".r)
      P.map(jsonNumber)(dblStr => JNumber(dblStr.toDouble))
    }

    def jarray: Parser[JSON] = {
      val csv = P.sep(value, P.char(','))
      val arr = P.skipRight(P.skipLeft(P.string("["), csv), P.string("]"))
      P.map(arr)(vals => JArray(vals.toIndexedSeq))
    }

    def jobject: Parser[JSON] = {
      val keyval = P.map2(P.skipRight(P.quoted, P.char(':')), value)((k,v) => (k,v))
      val csv = P.sep(keyval, P.char(','))
      val obj = P.skipRight(P.skipLeft(P.string("{"), csv), P.string("}"))
      P.map(obj)(kvs => JObject(kvs.toMap))
    }

    def value: Parser[JSON] =
      // NOTE: Deferred evaluation is built into the or function
      P.or(jnull,
        P.or(jtrue,
          P.or(jfalse,
            P.or(jstring,
              P.or(jnumber,
                P.or(jarray,
                  P.or(jobject, P.fail("Unable to parse input as JSON"))))))))

    def root: Parser[JSON] =
      P.surround(P.whitespace, P.endOfInput)(value)

    root
  }

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    def literal[A](p: Parser[A])(j: JSON): Parser[JSON] =
      jmap(P.token(p))(_ => j)

    /** Use token to remove trailing whitespace */
    def jmap[A](p: Parser[A])(f: A => JSON): Parser[JSON] =
      P.map(P.token(p))((a) => f(a))

    def jnull: Parser[JSON] = literal(P.string("null"))(JNull)

    def jtrue: Parser[JSON] = literal(P.string("true"))(JBool(true))

    def jfalse: Parser[JSON] = literal(P.string("false"))(JBool(false))
    
    def jstring: Parser[JSON] = jmap(P.quoted)(str => JString(str))
    
    def jnumber: Parser[JSON] = {
      val jsonNumber = P.regex("-?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][+-]?\\d+)?".r)
      jmap(jsonNumber)(dblStr => JNumber(dblStr.toDouble))
    }

    def jsep[A](p: Parser[A], s: Parser[Any]): Parser[List[A]] =
      P.token(P.sep(P.token(p), P.token(s)))

    def jsurround[A](open: Parser[Any], close: Parser[Any])(p: Parser[A]): Parser[A] =
      P.token(P.surround(P.token(open), P.token(close))(P.token(p)))

    def jarray: Parser[JSON] = {
      val csv = jsep(value, P.char(','))
      val arr = jsurround(P.string("["),P.string("]"))(csv)
      jmap(arr)(vals => JArray(vals.toIndexedSeq))
    }

    def jobject: Parser[JSON] = {
      val keyval = P.map2(P.skipRight(P.token(P.quoted), P.token(P.char(':'))), P.token(value))((k,v) => (k,v))
      val csv = jsep(keyval, P.char(','))
      val obj = jsurround(P.string("{"), P.string("}"))(csv)
      P.map(obj)(kvs => JObject(kvs.toMap))
    }

    def value: Parser[JSON] = {
      // NOTE: The parsers have to be listed this way because jarray and jobject
      // are defined in terms of value, which would cause this function to
      // StackOverflow if you don't defer evaluation.
      val parsers = Stream(
        () => jnull,
        () => jtrue,
        () => jfalse,
        () => jstring,
        () => jnumber,
        () => jarray,
        () => jobject
      )
      
      P.ors(parsers)("Unable to parse input as JSON")
    }

    def root: Parser[JSON] =
      P.surround(P.whitespace, P.endOfInput)(value)

    root
  }
}
