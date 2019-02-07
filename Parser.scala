package bisquit

object Parser {
  val keywordThen = Identifier.word("then")
  val keywordElse = Identifier.word("else")

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
    val (cond, keyword1) = nextThenEat(start, tail, keywordThen)(return _)
    val (pass, keyword2) = nextThenEat(keyword1, tail, keywordElse)(return _)
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

  def nextThenEat(last: Token, toks: Iterator[Token], expecting: Token)(
      handler: InvalidExpr => InvalidExpr
  ): (Expr, Token) = {
    val coming = next(last, toks)(handler)
    val eaten = eat(coming, expecting, toks) match {
      case (false, got) =>
        handler(InvalidExpr(List(got), List(expecting)))
      case (true, got) => got
    }

    (coming, eaten)
  }
}
