package xyz.minond.bisquit

import org.scalatest._

class ParserSpec extends FlatSpec with Matchers {
  def parse(src: String) =
    Parser.process(src, "parserspec").toList.collect {
      case Right(expr) => expr
      case Left(err)   => throw new Exception(err.toString)
    }

  it should "derive number types" in {
    Parser.deriveNumKind("1321") should be(Int)
    Parser.deriveNumKind("-1321") should be(Int)
    Parser.deriveNumKind("13.21") should be(Real)
    Parser.deriveNumKind("-13.21") should be(Real)
    Parser.deriveNumKind("0x1321") should be(Hex)
    Parser.deriveNumKind("-0x1321") should be(Hex)
    Parser.deriveNumKind("0b0001") should be(Bin)
    Parser.deriveNumKind("-0b001") should be(Bin)
  }

  it should "handle empty input" in {
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

  it should "parse application" in {
    parse("""xs()""") should be(
      List(
        App(
          Identifier("xs", "parserspec", 0),
          List.empty,
          CloseParen("parserspec", 3)
        )
      )
    )

    parse("""xs(a)""") should be(
      List(
        App(
          Identifier("xs", "parserspec", 0),
          List(Identifier("a", "parserspec", 3)),
          CloseParen("parserspec", 4)
        )
      )
    )

    parse("""xs(a,b,c,d)""") should be(
      List(
        App(
          Identifier("xs", "parserspec", 0),
          List(
            Identifier("a", "parserspec", 3),
            Identifier("b", "parserspec", 5),
            Identifier("c", "parserspec", 7),
            Identifier("d", "parserspec", 9)
          ),
          CloseParen("parserspec", 10)
        )
      )
    )
  }

  it should "function declaration" in {
    parse("""func xs() = 1""") should be(
      List(
        Binding(
          Function(
            Identifier("xs", "parserspec", 5),
            List(),
            CloseParen("parserspec", 8)
          ),
          Num("1", Int, "parserspec", 12),
          0
        )
      )
    )

    parse("""func xs(a, b, c) = 1""") should be(
      List(
        Binding(
          Function(
            Identifier("xs", "parserspec", 5),
            List(
              Variable(Identifier("a", "parserspec", 8), None),
              Variable(Identifier("b", "parserspec", 11), None),
              Variable(Identifier("c", "parserspec", 14), None)
            ),
            CloseParen("parserspec", 15)
          ),
          Num("1", Int, "parserspec", 19),
          0
        )
      )
    )

    parse("""func xs(a : int, b : int, c : int) = 1""") should be(
      List(
        Binding(
          Function(
            Identifier("xs", "parserspec", 5),
            List(
              Variable(
                Identifier("a", "parserspec", 8),
                Some(Type(Identifier("int", "parserspec", 12)))
              ),
              Variable(
                Identifier("b", "parserspec", 17),
                Some(Type(Identifier("int", "parserspec", 21)))
              ),
              Variable(
                Identifier("c", "parserspec", 26),
                Some(Type(Identifier("int", "parserspec", 30)))
              )
            ),
            CloseParen("parserspec", 33)
          ),
          Num("1", Int, "parserspec", 37),
          0
        )
      )
    )
  }
}
