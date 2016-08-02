package parse.json

trait JSON

object JSON {
  import parse._

  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  /*def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._
    val spaces = slice(many(char(' ')))
    Nothing
  }*/
}

object JSONExample {
  val nullTest = "null"
  val testOne = "{ \"abc\": \"def\", \"ghi\": 12 }"

  //def testNull() = Parsers.run(
}
