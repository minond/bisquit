package bisquit

sealed trait Token {
  def getPos: Int
  def getFile: String
}

abstract class PositionedToken(pos: Int, file: String) extends Token {
  def getPos = pos
  def getFile = file
}

case class InvalidToken(lexeme: String, pos: Int, file: String)
  extends PositionedToken(pos, file)

case class SingleQuote(pos: Int, file: String)
  extends PositionedToken(pos, file)
case class OpenParen(pos: Int, file: String) extends PositionedToken(pos, file)
case class CloseParen(pos: Int, file: String) extends PositionedToken(pos, file)
case class Colon(pos: Int, file: String) extends PositionedToken(pos, file)
case class Eq(pos: Int, file: String) extends PositionedToken(pos, file)
case class Pipe(pos: Int, file: String) extends PositionedToken(pos, file)
case class Underscore(pos: Int, file: String) extends PositionedToken(pos, file)
case class Arrow(pos: Int, file: String) extends PositionedToken(pos, file)

sealed trait Expr

sealed trait Scalar extends Token with Expr

case class Number(lexeme: String, pos: Int, file: String)
  extends PositionedToken(pos, file)
  with Scalar
case class Str(lexeme: String, pos: Int, file: String)
  extends PositionedToken(pos, file)
  with Scalar
case class Identifier(lexeme: String, pos: Int, file: String)
  extends PositionedToken(pos, file)
  with Scalar

sealed trait Bool extends Token with Scalar
case class True(pos: Int, file: String)
  extends PositionedToken(pos, file)
  with Bool
case class False(pos: Int, file: String)
  extends PositionedToken(pos, file)
  with Bool

