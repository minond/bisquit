package bisquit

object Parser {
  def parse(toks: Iterator[Token]): Iterator[Expr] =
    for (t <- toks)
      yield
        t match {
          case Identifier("true", pos, file)  => True(pos, file)
          case Identifier("false", pos, file) => False(pos, file)
          case scalar: Scalar                 => scalar

          case start @ Identifier("if", _, _) => parseCond(start, toks)

          // TODO finish rest of expressions
        }

  def parseCond(start: Token, tail: Iterator[Token]): Expr = {
    val cond = parse(tail).next
    if (!eat(Identifier.word("then"), tail)) {
      return InvalidExpr(List(cond), List(Identifier.word("then")))
    }

    val pass = parse(tail).next
    if (!eat(Identifier.word("else"), tail)) {
      return InvalidExpr(List(cond), List(Identifier.word("else")))
    }

    val fail = parse(tail).next
    Cond(cond, pass, fail, start.getPos)
  }

  def eat(expecting: Token, toks: Iterator[Token]): Boolean =
    if (!toks.hasNext)
      // TODO return EOF
      false
    else if (Lexer.eq(toks.next, expecting))
      true
    else
      // TODO return got
      false
}
