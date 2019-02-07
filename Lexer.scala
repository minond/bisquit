package bisquit

sealed trait Token

case class InvalidToken(lexeme: String) extends Token

case object SingleQuote extends Token
case object OpenParen extends Token
case object CloseParen extends Token
case object Colon extends Token
case object Eq extends Token
case object Pipe extends Token
case object Underscore extends Token
case object Arrow extends Token

sealed trait Expr extends Token

sealed trait Scalar extends Expr

case class Number(lexeme: String) extends Scalar
case class Str(lexeme: String) extends Scalar
case class Identifier(lexeme: String) extends Scalar

sealed trait Bool extends Scalar
case object True extends Bool
case object False extends Bool

object Lexer {
  type Predicate[T] = T => Boolean

  val op = Set('+', '-', '*', '&', '^', '%', '!', '\\', '|', '>', '<')

  def lex(str: String): Iterator[Token] = {
    val src = str.toList.toIterator.buffered
    for (c <- src if !c.isWhitespace)
      yield
        c match {
          case '(' => OpenParen
          case ')' => CloseParen
          case ':' => Colon
          case '|' => Pipe
          case '_' => Underscore

          // TODO handle escaped quotes
          case '"' =>
            Str(src.takeWhile(not(is('"'))).mkString)

          // TODO implement nicer peek method
          case '=' =>
            src.headOption match {
              case Some('>') => src.next; Arrow
              case _         => Eq
            }

          // TODO handle other number types
          case n
              if isDigit(n) || (is('-')(n) &&
                src.hasNext &&
                isDigit(src.head)) =>
            Number((n + consumeWhile(src, isDigit).mkString))

          case c if isLetter(c) =>
            Identifier(c + consumeWhile(src, isIdentifierTail).mkString)

          case c => InvalidToken(c + consumeWhile(src, isWord).mkString)
        }
  }

  def consumeWhile[T](
    src: BufferedIterator[T],
    predicate: Predicate[T]
  ): Iterator[T] = {
    def aux(buff: List[T]): List[T] =
      if (src.hasNext && predicate(src.head)) {
        val curr = src.head
        src.next
        aux(buff :+ curr)
      } else buff

    aux(List.empty).toIterator
  }

  def isIdentifierTail(c: Char): Boolean =
    isLetter(c) || isDigit(c) || c == '_'

  def isDigit(c: Char): Boolean =
    c >= '0' && c <= '9'

  def isLetter(c: Char): Boolean =
    (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

  def isSymbol(c: Char): Boolean =
    op.contains(c)

  def isParen(c: Char): Boolean =
    c == '(' || c == ')'

  def isWord(c: Char): Boolean =
    !c.isWhitespace && !isParen(c)

  def is[T](x: T): Predicate[T] =
    (y: T) => x == y

  def not[T](f: Predicate[T]): Predicate[T] =
    (x: T) => !f(x)
}
