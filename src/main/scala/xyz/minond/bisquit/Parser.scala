package xyz.minond.bisquit

import xyz.minond.bisquit.node._

object Parser {
  val wordThen = Identifier.word("then")
  val wordElse = Identifier.word("else")

  def parse(toks: Iterator[Token]): Iterator[Expr] =
    for (t <- toks)
      yield
        t match {
          case Identifier("true", file, start)  => True(file, start)
          case Identifier("false", file, start) => False(file, start)

          case start @ Identifier("if", _, _)  => parseCond(start, toks)
          case start @ Identifier("val", _, _) => parseVal(start, toks.buffered)

          case scalar: Scalar => scalar
          case id: Identifier => id

          case err: InvalidToken => InvalidExpr(List(err))

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

  def parseVal(start: Token, toks: BufferedIterator[Token]): Expr = {
    val name: Identifier = next(start, toks)(return _) match {
      case name: Identifier => name
      case err              => return UnexpectedExpr(err, "identifier")
    }

    val (typ, beforeEq) = maybeTyp(toks)(return _) match {
      case Left(err)              => return err
      case Right(typ @ Some(tok)) => (typ, tok)
      case Right(typ @ None)      => (typ, name)
    }

    val eqSign = expect[Eq](beforeEq, toks)(return _)

    val body = next(eqSign, toks)(return _)
    Binding(Varible(name, typ), body, start.getStart)
  }

  def withHandlerParseOptionalType(
      toks: Iterator[Token]
  )(handler: Error => Error): Either[Error, Option[Type]] =
    peek(toks) match {
      case Some(colon: Colon) =>
        toks.next
        next(colon, toks)(handler) match {
          case typ: Identifier => Right(Some(Type(typ)))
          case err             => Left(handler(UnexpectedExpr(err, "type annotation")))
        }

      case _ => Right(None)
    }

  // Peeks at the next token, if any, without moving forward.
  def peek(toks: Iterator[Token]): Option[Token] =
    toks.buffered.headOption

  // Safely returns the next expression and propagates errors to the error
  // handler. The error handler is also triggered with an EOF error when there
  // are no more tokens in the buffer.
  def next(last: Positioned, toks: Iterator[Token])(
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
      last: Positioned,
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
  def expect(last: Positioned, expecting: Token, toks: Iterator[Token])(
      handler: Error => Error
  ): Token =
    eat(last, toks) match {
      case got: Error =>
        handler(got)
      case got if !Token.eqv(got, expecting) =>
        handler(InvalidExpr(List(got), List(expecting)))
      case got => got
    }

  def expect[Expecting](last: Positioned, toks: Iterator[Token])(
      handler: Error => Error
  ): Token =
    eat(last, toks) match {
      case got: Error     => handler(got)
      case got: Expecting => got
      case got            => handler(InvalidExpr(List(got)))
    }
}
