package xyz.minond.bisquit.node

object Parser {
  val wordThen = Identifier.word("then")
  val wordElse = Identifier.word("else")

  def parse(toks: Iterator[Token]): Iterator[Either[Error, Expr]] =
    for (t <- toks)
      yield
        t match {
          case Identifier("true", file, start)  => Right(True(file, start))
          case Identifier("false", file, start) => Right(False(file, start))

          case start @ Identifier("if", _, _)  => parseCond(start, toks)
          case start @ Identifier("val", _, _) => parseVal(start, toks.buffered)

          case scalar: Scalar => Right(scalar)
          case id: Identifier => Right(id)

          case ExpectedMoreInput(file, pos) => Left(UnexpectedEOF(file, pos))
          case err: LexerError => Left(InvalidExpr(err))

          // TODO finish rest of expressions
        }

  def parseCond(start: Token, toks: Iterator[Token]): Either[Error, Expr] = {
    val cond = next(start, toks)(err => return Left(err))
    val key1 = expect(cond, wordThen, toks)(err => return Left(err))
    val pass = next(key1, toks)(err => return Left(err))
    val key2 = expect(pass, wordElse, toks)(err => return Left(err))
    val fail = next(key2, toks)(err => return Left(err))
    Right(Cond(cond, pass, fail, start.getStart))
  }

  def parseVal(
      start: Token,
      toks: BufferedIterator[Token]
  ): Either[Error, Expr] = {
    val name: Identifier = next(start, toks)(err => return Left(err)) match {
      case name: Identifier => name
      case err              => return Left(UnexpectedExpr(err, "identifier"))
    }

    val (typ, beforeEq) =
      parseOptionalType(toks)(err => return Left(err)) match {
        case Left(err)              => return Left(err)
        case Right(typ @ Some(tok)) => (typ, tok)
        case Right(typ @ None)      => (typ, name)
      }

    val eqSign = expect[Eq](beforeEq, toks)(err => return Left(err))

    val body = next(eqSign, toks)(err => return Left(err))
    Right(Binding(Variable(name, typ), body, start.getStart))
  }

  def parseOptionalType(toks: Iterator[Token])(
      errHandler: Error => Error
  ): Either[Error, Option[Type]] =
    peek(toks) match {
      case Some(colon: Colon) =>
        toks.next
        next(colon, toks)(errHandler) match {
          case typ: Identifier => Right(Some(Type(typ)))
          case err             => Left(errHandler(UnexpectedExpr(err, "type annotation")))
        }

      case _ => Right(None)
    }

  /** Peeks at the next token, if any, without moving forward.
    */
  def peek(toks: Iterator[Token]): Option[Token] =
    toks.buffered.headOption

  /** Safely returns the next expression and propagates errors to the error
    * handler. The error handler is also triggered with an EOF error when there
    * are no more tokens in the buffer.
    */
  def next(last: Positioned, toks: Iterator[Token])(
      errHandler: Error => Error
  ): Expr =
    if (!toks.hasNext)
      errHandler(UnexpectedEOF(last.getFile, last.getEnd))
    else
      parse(toks).next match {
        case Left(err) => errHandler(err)
        case Right(ok) => ok
      }

  /** Safely returns the next token from the tokens buffer. If the buffer is
    * empty an EOF error is returned.
    */
  def eat(last: Positioned, toks: Iterator[Token]): Token =
    if (!toks.hasNext)
      UnexpectedEOF(last.getFile, last.getEnd)
    else
      toks.next

  /** Safely returns and asserts the next token from the tokens buffer is equal
    * to the expected value. The error handler is triggered when the equality
    * assertion fails. Errors from processing the token buffer are propagated
    * to the handler as well.
    */
  def expect(last: Positioned, expecting: Token, toks: Iterator[Token])(
      errHandler: Error => Error
  ): Token =
    eat(last, toks) match {
      case got: Error =>
        errHandler(got)
      case got if !Token.eqv(got, expecting) =>
        errHandler(InvalidExpr(got, Some(expecting)))
      case got => got
    }

  /** Overloaded [[expect]] with type check instead of equivalence check.
    */
  def expect[Expecting](last: Positioned, toks: Iterator[Token])(
      errHandler: Error => Error
  ): Token =
    eat(last, toks) match {
      case got: Error     => errHandler(got)
      case got: Expecting => got
      case got            => errHandler(InvalidExpr(got))
    }
}
