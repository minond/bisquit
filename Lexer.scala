package bisquit

object Lexer {
  type Predicate[T] = T => Boolean

  val op = Set('+', '-', '*', '&', '^', '%', '!', '\\', '|', '>', '<')

  def lex(str: String, file: String): Iterator[Token] = {
    val src = str.toList.toIterator.zipWithIndex.buffered
    for ((c, pos) <- src if !c.isWhitespace)
      yield
        c match {
          case '(' => OpenParen(file, pos)
          case ')' => CloseParen(file, pos)
          case ':' => Colon(file, pos)
          case '|' => Pipe(file, pos)
          case '_' => Underscore(file, pos)

          // TODO handle escaped quotes
          case '"' =>
            val str = consumeWhile(src, not(is('"'))).mkString
            if (!src.hasNext)
              InvalidToken(str, file, pos)
            else
              src.next match {
                case ('"', _) => Str(str, file, pos)
                case (err, _) => InvalidToken(err.toString, file, pos)
              }

          // TODO implement nicer peek method
          case '=' =>
            src.headOption match {
              case Some(('>', _)) => src.next; Arrow(file, pos)
              case _              => Eq(file, pos)
            }

          // TODO handle other number types
          case n
              if isDigit(n) || (is('-')(n) &&
                src.hasNext &&
                isDigit(src.head._1)) =>
            Number((n + consumeWhile(src, isDigit).mkString), file, pos)

          case c if isLetter(c) =>
            Identifier(
              c + consumeWhile(src, isIdentifierTail).mkString,
              file,
              pos
            )

          case c =>
            InvalidToken(c + consumeWhile(src, isWord).mkString, file, pos)
        }
  }

  def consumeWhile[T](
      src: BufferedIterator[(T, Int)],
      predicate: Predicate[T]
  ): Iterator[T] = {
    def aux(buff: List[T]): List[T] =
      if (src.hasNext && predicate(src.head._1)) {
        val curr = src.head._1
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

  def eq(lhs: Token, rhs: Token): Boolean =
    (lhs, rhs) match {
      case (Identifier(lexeme1, _, _), Identifier(lexeme2, _, _))
          if lexeme1 == lexeme2 =>
        true

      case _ => false
    }
}
