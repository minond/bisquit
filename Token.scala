package bisquit

sealed trait Token {
  def getPos: Int
  def getFile: String
}

abstract class Positioned(pos: Int, file: String) {
  def getPos = pos
  def getFile = file
}

case class InvalidToken(lexeme: String, pos: Int, file: String)
    extends Positioned(pos, file)
    with Token

case class EOF(pos: Int, file: String) extends Positioned(pos, file) with Token
case class SingleQuote(pos: Int, file: String)
    extends Positioned(pos, file)
    with Token
case class OpenParen(pos: Int, file: String)
    extends Positioned(pos, file)
    with Token
case class CloseParen(pos: Int, file: String)
    extends Positioned(pos, file)
    with Token
case class Colon(pos: Int, file: String)
    extends Positioned(pos, file)
    with Token
case class Eq(pos: Int, file: String) extends Positioned(pos, file) with Token
case class Pipe(pos: Int, file: String) extends Positioned(pos, file) with Token
case class Underscore(pos: Int, file: String)
    extends Positioned(pos, file)
    with Token
case class Arrow(pos: Int, file: String)
    extends Positioned(pos, file)
    with Token

sealed trait Expr extends Token

case class InvalidExpr(got: List[Token], expected: List[Token])
    extends Positioned(got.head.getPos, got.head.getFile)
    with Expr

sealed trait Scalar extends Expr

case class Number(lexeme: String, pos: Int, file: String)
    extends Positioned(pos, file)
    with Scalar
case class Str(lexeme: String, pos: Int, file: String)
    extends Positioned(pos, file)
    with Scalar

sealed trait Bool extends Scalar
case class True(pos: Int, file: String) extends Positioned(pos, file) with Bool
case class False(pos: Int, file: String) extends Positioned(pos, file) with Bool

case class Identifier(lexeme: String, pos: Int, file: String)
    extends Positioned(pos, file)
    with Expr

object Identifier {
  def word(lexeme: String) = Identifier(lexeme, -1, "?")
}

case class Cond(cond: Expr, pass: Expr, fail: Expr, pos: Int)
    extends Positioned(pos, cond.getFile)
    with Expr
