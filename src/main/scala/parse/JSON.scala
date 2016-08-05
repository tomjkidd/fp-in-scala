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

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

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

    def value:Parser[JSON] =
      // TODO: Get this to work...
      //P.or(List(jnull, jtrue, jfalse, jstring, jnumber, jarray))
      P.or(jnull,
        P.or(jtrue,
          P.or(jfalse,
            P.or(jstring,
              P.or(jnumber,
                P.or(jarray, jobject))))))

    value
  }
}

/*
 JSON
 value := string
        | number
        | object
        | array
        | true
        | false
        | null

 */
