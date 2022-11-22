package com.agilogy.wapl

import Json.{JsonArray, JsonBoolean}
import scala.language.postfixOps

object JsonParser:

  private val whiteSpaceChars = Set(' ', '\n', '\r', '\t')

  val whitespace: Parser[Unit] = (s, position) =>
    Right(position + s.substring(position).takeWhile(c => whiteSpaceChars.contains(c)).length, ())

  val trueBoolean: Parser[JsonBoolean] = string("true").map(_ => JsonBoolean(true))
  val falseBoolean: Parser[JsonBoolean] = string("false").map(_ => JsonBoolean(false))
  val boolean: Parser[JsonBoolean] = trueBoolean | falseBoolean

  val arrayItems: Parser[List[JsonBoolean]] = ((boolean <* whitespace <* string(",") <* whitespace) | boolean)*
  val array: Parser[JsonArray] =
    (string("[") *> whitespace *> arrayItems <* whitespace <* string("]"))
      .map(JsonArray)
