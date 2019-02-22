package xyz.minond.bisquit.node

abstract class Positioned(file: String, start: Int, end: Int) {
  def getFile = file
  def getStart = start
  def getEnd = end
}

case class Selection(file: String, start: Int, end: Int)
    extends Positioned(file, start, end)

object Positioned {
  def at(pos: Positioned) =
    Selection(pos.getFile, pos.getStart, pos.getEnd)
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

      case (_: Eq, _: Eq)                 => true
      case (_: Colon, _: Colon)           => true
      case (_: Comma, _: Comma)           => true
      case (_: OpenParen, _: OpenParen)   => true
      case (_: CloseParen, _: CloseParen) => true

      case _ => false
    }
}

case class EOF(file: String, start: Int)
    extends Positioned(file, start, start)
    with Token
case class OpenParen(file: String, start: Int)
    extends Positioned(file, start, start + 1)
    with Token
case class CloseParen(file: String, start: Int)
    extends Positioned(file, start, start + 1)
    with Token
case class Comma(file: String, start: Int)
    extends Positioned(file, start, start + 1)
    with Token
case class Colon(file: String, start: Int)
    extends Positioned(file, start, start + 1)
    with Token
case class Eq(file: String, start: Int)
    extends Positioned(file, start, start + 1)
    with Token

// Errors are not a part of an AST but they do have contextual information to
// correctly place them in source code.
sealed trait Error extends Positioned

// Returned by the lexer when an unsupported token is encounterd. This is a
// proper Token that should be handled by the parser.
case class UnknownToken(lexeme: String, file: String, start: Int)
    extends Positioned(file, start, start + lexeme.length)
    with Token
    with Error

// Returned by the lexer when a certain token was expected but another one was
// encountered. For example, a mis-matching closing quote. This is a proper
// Token that should be handled by the parser.
case class UnexpectedToken(
    lexeme: String,
    msg: String,
    file: String,
    start: Int
) extends Positioned(file, start, start + lexeme.size)
    with Token
    with Error

// General purpose error returned by the parser. Required to include context in
// the form on the token where the expression was identified as being invalid
// and may optionally include information as to what was expected in its place.
case class InvalidExpr(got: Token, expected: Option[Token] = None)
    extends Positioned(got.getFile, got.getStart, got.getEnd)
    with Error

// Returned by the parser or lexer when an EOF or end of stream is encountered
// before a whole expression can be completely parsed.
case class UnexpectedEOF(file: String, pos: Int)
    extends Positioned(file, pos, pos)
    with Error

// Returned by the parser when an expression is properly parsed but is it not
// of the correct type. For example, when a number is placed where an
// identifier is expected.
case class UnexpectedExpr(token: Token, msg: String)
    extends Positioned(token.getFile, token.getStart, token.getEnd)
    with Error

sealed trait Expr extends Positioned with Token
sealed trait Scalar extends Expr

sealed trait NumKind
case object Int extends NumKind
case object Real extends NumKind
case object Hex extends NumKind
case object Bin extends NumKind

case class Num(lexeme: String, kind: NumKind, file: String, start: Int)
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

case class App(fn: Identifier, args: List[Expr], closeParen: Positioned)
    extends Positioned(fn.getFile, fn.getStart, closeParen.getEnd)
    with Expr
