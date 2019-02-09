package bisquit

sealed trait Token {
  def getFile: String
  def getStart: Int
  def getEnd: Int
}

object Token {
  def eq(lhs: Token, rhs: Token): Boolean =
    (lhs, rhs) match {
      case (Identifier(lexeme1, _, _), Identifier(lexeme2, _, _))
          if lexeme1 == lexeme2 =>
        true

      case _ => false
    }
}

abstract class Positioned(file: String, start: Int, end: Int) {
  def getFile = file
  def getStart = start
  def getEnd = end
}

// TODO also need a way to identify what we actually needed, maybe add an
// MissingToken type?
case class InvalidToken(lexeme: String, file: String, start: Int)
    extends Positioned(file, start, start + lexeme.length)
    with Token

case class EOF(file: String, pos: Int)
    extends Positioned(file, pos, pos)
    with Token
case class SingleQuote(file: String, start: Int)
    extends Positioned(file, start, start + 1)
    with Token
case class OpenParen(file: String, start: Int)
    extends Positioned(file, start, start + 1)
    with Token
case class CloseParen(file: String, start: Int)
    extends Positioned(file, start, start + 1)
    with Token
case class Colon(file: String, start: Int)
    extends Positioned(file, start, start + 1)
    with Token
case class Eq(file: String, start: Int)
    extends Positioned(file, start, start + 1)
    with Token
case class Pipe(file: String, start: Int)
    extends Positioned(file, start, start + 1)
    with Token
case class Underscore(file: String, start: Int)
    extends Positioned(file, start, start + 1)
    with Token
case class Arrow(file: String, start: Int)
    extends Positioned(file, start, start + 2)
    with Token

sealed trait Expr extends Token
sealed trait Error extends Expr

case class InvalidExpr(got: List[Token], expected: List[Token])
    extends Positioned(got.head.getFile, got.head.getStart, got.head.getEnd)
    with Expr
    with Error
case class UnexpectedEOF(file: String, pos: Int)
    extends Positioned(file, pos, pos)
    with Expr
    with Error

sealed trait Scalar extends Expr

case class Number(lexeme: String, file: String, start: Int)
    extends Positioned(file, start, start + lexeme.length)
    with Scalar
case class Str(lexeme: String, file: String, start: Int)
    extends Positioned(file, start, start + lexeme.length + 2)
    with Scalar

sealed trait Bool extends Scalar
case class True(file: String, start: Int)
    extends Positioned(file, start, start + 4)
    with Bool
case class False(file: String, start: Int)
    extends Positioned(file, start, start + 5)
    with Bool

case class Identifier(lexeme: String, file: String, start: Int)
    extends Positioned(file, start, start + lexeme.length)
    with Expr

object Identifier {
  def word(lexeme: String) = Identifier(lexeme, "", 0)
}

case class Cond(cond: Expr, pass: Expr, fail: Expr, start: Int)
    extends Positioned(cond.getFile, start, fail.getEnd)
    with Expr
