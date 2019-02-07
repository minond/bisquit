package bisquit

object Parser {
  def parse(toks: Iterator[Token]): Iterator[Expr] =
    for (t <- toks)
      yield
        t match {
          case Identifier("true", file, start)  => True(file, start)
          case Identifier("false", file, start) => False(file, start)
          case scalar: Scalar                   => scalar

          case start @ Identifier("if", _, _) => parseCond(start, toks)

          case err: InvalidToken => InvalidExpr(List(err), List.empty)

          // TODO finish rest of expressions
        }

  def parseCond(start: Token, tail: Iterator[Token]): Expr = {
    val cond = next(start, tail)(return _)
    val keyword1 = eat(cond, Identifier.word("then"), tail) match {
      case (false, got) =>
        return InvalidExpr(List(got), List(Identifier.word("then")))
      case (true, got) => got
    }

    val pass = next(keyword1, tail)(return _)
    val keyword2 = eat(pass, Identifier.word("else"), tail) match {
      case (false, got) =>
        return InvalidExpr(List(got), List(Identifier.word("else")))
      case (true, got) => got
    }

    val fail = next(keyword2, tail)(return _)
    Cond(cond, pass, fail, start.getStart)
  }

  def next(last: Token, toks: Iterator[Token])(
      handler: InvalidExpr => InvalidExpr
  ): Expr =
    if (!toks.hasNext)
      handler(InvalidExpr(List(EOF(last.getFile, last.getEnd)), List.empty))
    else
      parse(toks).next

  def eat(
      last: Token,
      expecting: Token,
      toks: Iterator[Token]
  ): (Boolean, Token) =
    if (!toks.hasNext)
      (false, EOF(last.getFile, last.getEnd))
    else {
      val got = toks.next
      (Lexer.eq(got, expecting), got)
    }
}
