package bisquit

object Parser {
  type Tokens = Iterator[Token]

  def parse(toks: Tokens): Iterator[Expr] =
    for (t <- toks)
      yield
        t match {
          case Identifier("true", pos, file)  => True(pos, file)
          case Identifier("false", pos, file) => False(pos, file)
          case start @ Identifier("if", _, _) => parseCond(start, toks)
          case scalar: Scalar                 => scalar
          // TODO rest of exprs
          case tok => InvalidExpr(List(tok), List.empty)
        }

  def parseCond(start: Token, tail: Tokens): Expr = {
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

  def eat(expecting: Token, toks: Tokens): Boolean =
    if (!toks.hasNext)
      // TODO return EOF
      false
    else if (Lexer.eq(toks.next, expecting))
      true
    else
      // TODO return got
      false
}
