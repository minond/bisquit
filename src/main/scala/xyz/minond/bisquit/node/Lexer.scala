package xyz.minond.bisquit.node

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
          case '=' => Eq(file, pos)

          case '"' => lexStr('"', src, file, pos)
          case '`' => lexStr('`', src, file, pos)

          // TODO handle other number types
          case n
              if isDigit(n) || (is('-')(n) &&
                src.hasNext &&
                isDigit(src.head._1)) =>
            Num((n + consumeWhile(src, isDigit).mkString), file, pos)

          case c if isLetter(c) =>
            Identifier(
              c + consumeWhile(src, isIdentifierTail).mkString,
              file,
              pos
            )

          case c =>
            UnknownToken(c + consumeWhile(src, isWord).mkString, file, pos)
        }
  }

  def lexStr(
      end: Char,
      src: BufferedIterator[(Char, Int)],
      file: String,
      pos: Int
  ): Token = {
    val str = consumeWhile(src, not(is(end))).mkString
    if (!src.hasNext)
      EOF(file, str.size + pos)
    else
      src.next match {
        case (_end, _) if _end == end => Str(str, file, pos)
        case (bad, _) =>
          UnexpectedToken(
            bad.toString,
            s"expecting ${end} at end of string",
            file,
            pos
          )
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
}
