package com.agilogy.wapl

import Json.{JsonArray, JsonBoolean}
import scala.language.postfixOps

object JsonParser:

  val whitespace: Parser[Unit] = ((string(" ") | string("\n") | string("\r") | string("\t"))*).as(())
  val trueBoolean: Parser[JsonBoolean] = string("true").as(JsonBoolean(true))
  val falseBoolean: Parser[JsonBoolean] = string("false").as(JsonBoolean(false))
  val boolean: Parser[JsonBoolean] = trueBoolean | falseBoolean

  val arrayItems: Parser[List[JsonBoolean]] = ((boolean <* whitespace <* string(",") <* whitespace) | boolean)*
  val array: Parser[JsonArray] =
    (string("[") *> whitespace *> arrayItems <* whitespace <* string("]"))
      .map(JsonArray.apply)
