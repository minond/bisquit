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

          // TODO finish rest of expressions
        }

  def parseCond(start: Token, tail: Iterator[Token]): Expr = {
    val cond = next(tail)
    if (!cond.isInstanceOf[Expr]) {
      // TODO expecting expr but no way of stating that, empty for now
      return InvalidExpr(List.empty, List(cond))
    }
    eat(Identifier.word("then"), tail) match {
      case (false, got) =>
        return InvalidExpr(List(got), List(Identifier.word("then")))
      case (true, _) =>
    }

    val pass = next(tail)
    if (!pass.isInstanceOf[Expr]) {
      // TODO expecting expr but no way of stating that, empty for now
      return InvalidExpr(List.empty, List(pass))
    }
    eat(Identifier.word("else"), tail) match {
      case (false, got) =>
        return InvalidExpr(List(got), List(Identifier.word("else")))
      case (true, _) =>
    }

    val fail = parse(tail).next
    if (!fail.isInstanceOf[Expr]) {
      // TODO expecting expr but no way of stating that, empty for now
      return InvalidExpr(List.empty, List(fail))
    }

    Cond(cond, pass, fail, start.getStart)
  }

  def next(toks: Iterator[Token]): Expr =
    if (!toks.hasNext)
      InvalidExpr(List(EOF("", -1)), List.empty)
    else
      parse(toks).next

  def eat(expecting: Token, toks: Iterator[Token]): (Boolean, Token) =
    if (!toks.hasNext)
      (false, EOF("", -1))
    else {
      val got = toks.next
      (Lexer.eq(got, expecting), got)
    }
}
