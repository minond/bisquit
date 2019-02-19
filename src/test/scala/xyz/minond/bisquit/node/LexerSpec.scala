package xyz.minond.bisquit.node

import org.scalatest._

class LexerSpec extends FlatSpec with Matchers {
  def lex(src: String) =
    Lexer.lex(src, "parserspec").toList

  "The Lexer" should "handle empty input" in {
    lex("") should be(List())
    lex(" ") should be(List())
    lex("          ") should be(List())
    lex("				") should be(List())
    lex("""


    """) should be(List())
  }

  it should "derive number types" in {
    Lexer.deriveNumKind("1321") should be(Int)
    Lexer.deriveNumKind("-1321") should be(Int)
    Lexer.deriveNumKind("13.21") should be(Real)
    Lexer.deriveNumKind("-13.21") should be(Real)
    Lexer.deriveNumKind("0x1321") should be(Hex)
    Lexer.deriveNumKind("-0x1321") should be(Hex)
    Lexer.deriveNumKind("0b0001") should be(Bin)
    Lexer.deriveNumKind("-0b001") should be(Bin)
  }
}
