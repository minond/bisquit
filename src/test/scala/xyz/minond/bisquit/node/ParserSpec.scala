package xyz.minond.bisquit.node

import org.scalatest._

class ParserSpec extends FlatSpec with Matchers {
  def parse(src: String) =
    Parser.parse(Lexer.lex(src, "parserspec")).toList.collect {
      case Right(expr) => expr
      case Left(err)   => throw new Exception(Printer.error(err))
    }

  "The Parser" should "handle empty input" in {
    parse("") should be(List())
    parse(" ") should be(List())
    parse("          ") should be(List())
    parse("				") should be(List())
    parse("""


    """) should be(List())
  }

  it should "parse boolean values" in {
    parse("""true""") should be(List(True("parserspec", 0)))
    parse("""false""") should be(List(False("parserspec", 0)))
  }

  it should "parse string values" in {
    parse(""""true"""") should be(List(Str("true", "parserspec", 0)))
    parse(""""false"""") should be(List(Str("false", "parserspec", 0)))
  }

  it should "parse integer values" in {
    parse("""0""") should be(List(Num("0", Int, "parserspec", 0)))
    parse("""123""") should be(List(Num("123", Int, "parserspec", 0)))
    parse("""-0""") should be(List(Num("-0", Int, "parserspec", 0)))
    parse("""-123""") should be(List(Num("-123", Int, "parserspec", 0)))
  }

  it should "parse if expressions" in {
    parse(""" if true
            | then 1
            | else 2
      """.stripMargin) should be(
      List(
        Cond(
          True("parserspec", 4),
          Num("1", Int, "parserspec", 15),
          Num("2", Int, "parserspec", 23),
          1
        )
      )
    )
  }

  it should "parse nested if expressions" in {
    parse(""" if true
            | then
            |   if false
            |   then 1
            |   else 2
            | else
            |   if true
            |   then 3
            |   else 4
      """.stripMargin) should be(
      List(
        Cond(
          True("parserspec", 4),
          Cond(
            False("parserspec", 21),
            Num("1", Int, "parserspec", 35),
            Num("2", Int, "parserspec", 45),
            18
          ),
          Cond(
            True("parserspec", 59),
            Num("3", Int, "parserspec", 72),
            Num("4", Int, "parserspec", 82),
            56
          ),
          1
        )
      )
    )
  }

  it should "parse let expression" in {
    parse(""" let
            |   val a = 1
            |   val b = 2
            |   val c = 3
            | in abc
      """.stripMargin) should be(
      List(
        Let(
          List(
            Binding(
              Variable(Identifier("a", "parserspec", 12), None),
              Num("1", Int, "parserspec", 16),
              8
            ),
            Binding(
              Variable(Identifier("b", "parserspec", 25), None),
              Num("2", Int, "parserspec", 29),
              21
            ),
            Binding(
              Variable(Identifier("c", "parserspec", 38), None),
              Num("3", Int, "parserspec", 42),
              34
            )
          ),
          Identifier("abc", "parserspec", 48),
          1
        )
      )
    )
  }

  it should "parse val statements" in {
    parse("""val a = 1""") should be(
      List(
        Binding(
          Variable(Identifier("a", "parserspec", 4), None),
          Num("1", Int, "parserspec", 8),
          0
        )
      )
    )
  }

  it should "parse val statements with types" in {
    parse("""val a : int8 = 1""") should be(
      List(
        Binding(
          Variable(
            Identifier("a", "parserspec", 4),
            Some(Type(Identifier("int8", "parserspec", 8)))
          ),
          Num("1", Int, "parserspec", 15),
          0
        )
      )
    )
  }
}
