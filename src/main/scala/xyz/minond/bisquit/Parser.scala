package xyz.minond.bisquit

import xyz.minond.bisquit.node._

object Parser {
  val wordThen = Identifier.word("then")
  val wordElse = Identifier.word("else")
  val wordEq = Eq("", -1)

  def parse(toks: Iterator[Token]): Iterator[Expr] =
    for (t <- toks)
      yield
        t match {
          case Identifier("true", file, start)  => True(file, start)
          case Identifier("false", file, start) => False(file, start)

          case start @ Identifier("if", _, _)  => parseCond(start, toks)
          case start @ Identifier("val", _, _) => parseValBinding(start, toks)

          case scalar: Scalar => scalar
          case id: Identifier => id

          case err: InvalidToken => InvalidExpr(List(err), List.empty)

          // TODO finish rest of expressions
        }

  def parseCond(start: Token, toks: Iterator[Token]): Expr = {
    val cond = next(start, toks)(return _)
    val key1 = expect(cond, wordThen, toks)(return _)
    val pass = next(key1, toks)(return _)
    val key2 = expect(pass, wordElse, toks)(return _)
    val fail = next(key2, toks)(return _)
    Cond(cond, pass, fail, start.getStart)
  }

  def parseValBinding(start: Token, toks: Iterator[Token]): Expr = {
    val name = next(start, toks)(return _)
    val key1 = expect(name, wordEq, toks)(return _)
    val typ = None
    name match {
      case name: Identifier =>
        val body = next(key1, toks)(return _)
        Binding(Varible(name, typ), body, start.getStart)

      case got => InvalidExpr(List(got), List.empty)
    }
  }

  // Safely returns the next expression and propagates errors to the error
  // handler. The error handler is also triggered with an EOF error when there
  // are no more tokens in the buffer.
  def next(last: Token, toks: Iterator[Token])(
      handler: Error => Error
  ): Expr =
    if (!toks.hasNext)
      handler(UnexpectedEOF(last.getFile, last.getEnd))
    else
      parse(toks).next match {
        case err: Error => handler(err)
        case ok         => ok
      }

  // Safely returns the next token from the tokens buffer. If the buffer is
  // empty an EOF error is returned.
  def eat(
      last: Token,
      toks: Iterator[Token]
  ): Token =
    if (!toks.hasNext)
      UnexpectedEOF(last.getFile, last.getEnd)
    else
      toks.next

  // Safely returns and asserts the next token from the tokens buffer is equal
  // to the expected value. The error handler is triggered when the equality
  // assertion fails. Errors from processing the token buffer are propagated to
  // the handler as well.
  def expect(
      last: Token,
      expecting: Token,
      toks: Iterator[Token]
  )(
      handler: Error => Error
  ): Token =
    eat(last, toks) match {
      case got: Error =>
        handler(got)
      case got if !Token.eq(got, expecting) =>
        handler(InvalidExpr(List(got), List(expecting)))
      case got => got
    }
}
