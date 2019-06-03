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

  it should "parse val expressions" in {
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

  it should "parse val expressions with types" in {
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

  it should "parse function declaration" in {
    parse("""func xs1() = 1""") should be(
      List(
        Binding(
          Function(
            Identifier("xs1", "parserspec", 5),
            List(),
            None,
            Eq("parserspec", 11)
          ),
          Num("1", Int, "parserspec", 13),
          0
        )
      )
    )

    parse("""func xs2(a, b, c) = 1""") should be(
      List(
        Binding(
          Function(
            Identifier("xs2", "parserspec", 5),
            List(
              Variable(Identifier("a", "parserspec", 9), None),
              Variable(Identifier("b", "parserspec", 12), None),
              Variable(Identifier("c", "parserspec", 15), None)
            ),
            None,
            Eq("parserspec", 18)
          ),
          Num("1", Int, "parserspec", 20),
          0
        )
      )
    )

    parse("""func xs3(a : int, b : int, c : int) = 1""") should be(
      List(
        Binding(
          Function(
            Identifier("xs3", "parserspec", 5),
            List(
              Variable(
                Identifier("a", "parserspec", 9),
                Some(Type(Identifier("int", "parserspec", 13)))
              ),
              Variable(
                Identifier("b", "parserspec", 18),
                Some(Type(Identifier("int", "parserspec", 22)))
              ),
              Variable(
                Identifier("c", "parserspec", 27),
                Some(Type(Identifier("int", "parserspec", 31)))
              )
            ),
            None,
            Eq("parserspec", 36)
          ),
          Num("1", Int, "parserspec", 38),
          0
        )
      )
    )

    parse("""func xs4() : int = 1""") should be(
      List(
        Binding(
          Function(
            Identifier("xs4", "parserspec", 5),
            List(),
            Some(Type(Identifier("int", "parserspec", 13))),
            Eq("parserspec", 17)
          ),
          Num("1", Int, "parserspec", 19),
          0
        )
      )
    )
  }
}
