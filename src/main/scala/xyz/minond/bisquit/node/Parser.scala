package xyz.minond.bisquit.node

import scala.reflect.{classTag, ClassTag}

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

          case err: UnexpectedEOF   => Left(err)
          case err: UnknownToken    => Left(InvalidExpr(err))
          case err: UnexpectedToken => Left(InvalidExpr(err))

          // TODO finish rest of expressions
        }

  def parseCond(start: Token, toks: Iterator[Token]): Either[Error, Expr] =
    for {
      cond <- next(start, toks).right
      key1 <- expect(cond, wordThen, toks).right
      pass <- next(key1, toks).right
      key2 <- expect(pass, wordElse, toks).right
      fail <- next(key2, toks).right
    } yield Cond(cond, pass, fail, start.getStart)

  def parseVal(
      start: Token,
      toks: BufferedIterator[Token]
  ): Either[Error, Expr] =
    for {
      name <- expect[Identifier](start, toks).right
      typ <- parseOptionalType(toks, name).right
      sign <- expect[Eq](typ._2, toks).right
      body <- next(sign, toks).right
    } yield Binding(Variable(name, typ._1), body, start.getStart)

  def parseOptionalType(
      toks: Iterator[Token],
      last: Token
  ): Either[Error, (Option[Type], Token)] =
    peek(toks) match {
      case Some(colon: Colon) =>
        toks.next
        next(colon, toks).flatMap { expr =>
          expr match {
            case typ: Identifier => Right((Some(Type(typ)), typ))
            case err             => Left(UnexpectedExpr(err, "type annotation"))
          }
        }

      case _ => Right((None, last))
    }

  /** Simple identification function used to clean up [[Either]] folding done
    * during parsing expressions.
    */
  def pass[X](x: X): X = x

  /** Peeks at the next token, if any, without moving forward.
    */
  def peek(toks: Iterator[Token]): Option[Token] =
    toks.buffered.headOption

  /** Safely returns the next token from the tokens buffer. If the buffer is
    * empty an EOF error is returned.
    */
  def eat(last: Positioned, toks: Iterator[Token]): Token =
    if (!toks.hasNext)
      UnexpectedEOF(last.getFile, last.getEnd)
    else
      toks.next

  /** Safely returns the next parsable expression. A [[Left[Error]] is returned
    * if the token buffer is empty ro when an [[Error]] propagates from parsing
    * the next expression.
    */
  def next(last: Positioned, toks: Iterator[Token]): Either[Error, Expr] =
    if (!toks.hasNext)
      Left(UnexpectedEOF(last.getFile, last.getEnd))
    else
      parse(toks).next

  /** Safely returns and asserts the next token from the tokens buffer is equal
    * to the expected value. A [[Left[Error]]] is returned when the equality
    * assertion fails or when an [[Error]] propagates from the token buffer
    * processing.
    */
  def expect(
      last: Positioned,
      expecting: Token,
      toks: Iterator[Token]
  ): Either[Error, Token] =
    eat(last, toks) match {
      case got: Error => Left(got)
      case got if !Token.eqv(got, expecting) =>
        Left(InvalidExpr(got, Some(expecting)))
      case got => Right(got)
    }

  /** Overloaded [[expect]] with type check instead of equivalence check.
    */
  def expect[Expecting: ClassTag](
      last: Positioned,
      toks: Iterator[Token]
  ): Either[Error, Expecting] =
    eat(last, toks) match {
      case got: Error => Left(got)
      case got if classTag[Expecting].runtimeClass.isInstance(got) =>
        Right(got.asInstanceOf[Expecting])
      case got => Left(InvalidExpr(got))
    }
}
