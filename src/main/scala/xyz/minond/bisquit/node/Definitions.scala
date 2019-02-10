package xyz.minond.bisquit.node

abstract class Positioned(file: String, start: Int, end: Int) {
  def getFile = file
  def getStart = start
  def getEnd = end
}

sealed trait Token extends Positioned {
  def getFile: String
  def getStart: Int
  def getEnd: Int
}

object Token {
  def eqv(lhs: Token, rhs: Token): Boolean =
    (lhs, rhs) match {
      case (Identifier(lexeme1, _, _), Identifier(lexeme2, _, _))
          if lexeme1 == lexeme2 =>
        true

      case (_: Eq, _: Eq)       => true
      case (_: Colon, _: Colon) => true

      case _ => false
    }
}

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

sealed trait Expr extends Positioned with Token
sealed trait Error extends Expr

case class InvalidExpr(got: Token, expected: Option[Token] = None)
    extends Positioned(got.getFile, got.getStart, got.getEnd)
    with Expr
    with Error
case class UnexpectedEOF(file: String, pos: Int)
    extends Positioned(file, pos, pos)
    with Expr
    with Error
case class UnexpectedExpr(token: Token, msg: String)
    extends Positioned(token.getFile, token.getStart, token.getEnd)
    with Expr
    with Error

sealed trait Scalar extends Expr

case class Num(lexeme: String, file: String, start: Int)
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

case class Type(name: Identifier)
    extends Positioned(name.getFile, name.getStart, name.getEnd)
case class Argument(name: Identifier, typ: Option[Type])
    extends Positioned(name.getFile, name.getStart, typ.getOrElse(name).getEnd)

sealed trait Declaration extends Positioned
case class Variable(name: Identifier, typ: Option[Type])
    extends Positioned(name.getFile, name.getStart, typ.getOrElse(name).getEnd)
    with Declaration
case class Function(name: Identifier, args: List[Argument], typ: Option[Type])
    extends Positioned(name.getFile, name.getStart, typ.getOrElse(name).getEnd)
    with Declaration

sealed trait Stmt extends Expr

case class Binding(decl: Declaration, body: Expr, start: Int)
    extends Positioned(decl.getFile, decl.getStart, body.getEnd)
    with Stmt

case class Let(bindings: List[Binding], body: Expr, start: Int)
    extends Positioned(body.getFile, start, body.getEnd)
    with Expr
