package com.agilogy.wapl

import Json.*

import com.agilogy.wapl.JsonParser.array
import org.scalatest.funsuite.AnyFunSuite

import scala.util.Random

class JsonParserTest extends AnyFunSuite :

  import JsonParser.*

  test("Parse empty array") {
    assert(array("[]", 0) == Right((2,JsonArray(List.empty))))
  }

  private val random = new Random()

  test("Parse boolean") {
    val bool = random.nextBoolean()
    assert(boolean(bool.toString, 0) == Right((bool.toString.length, JsonBoolean(bool))))
  }

  test("Parse array of booleans") {
    assert(array("[true,false,true]", 0) == Right((17,JsonArray(List(JsonBoolean(true),JsonBoolean(false),JsonBoolean(true))))))
  }

  test("Parse array of booleans with spaces") {
    assert(array("[ true, false, true ]", 0) == Right((21,JsonArray(List(JsonBoolean(true),JsonBoolean(false),JsonBoolean(true))))))
  }

  test("Parse empty array with whitespace") {
    val ws = " " * random.between(1, 5)
    assert(array(s"[$ws]", 0) == Right((2 + ws.length, JsonArray(List.empty))))
  }

  test("Parse empty array failure") {
    val input = "["
    assert(array(input, 0) == Left(ParseError(input, 1, expected = "]")))
  }

  test("Parse token") {
    assert(string("[")("[", 0) == Right((1, ())))
  }

  test("Parse token failure") {
    val input = "notTheStartArrayToken"
    assert(string("[")(input, 0) == Left(ParseError(input, 0, expected = "[")))
  }


