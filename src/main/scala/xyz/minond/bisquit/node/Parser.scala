package xyz.minond.bisquit.node

import scala.reflect.{classTag, ClassTag}

object Parser {
  val wordThen = Identifier("then", "<internal>", 0)
  val wordElse = Identifier("else", "<internal>", 0)
  val wordIn = Identifier("in", "<internal>", 0)

  val charComma = Comma("<internal>", 0)
  val charCloseParen = CloseParen("<internal>", 0)

  def parse(toks: BufferedIterator[Token]): Iterator[Either[Error, Expr]] =
    for (t <- toks)
      yield
        t match {
          case Identifier("true", file, start)  => Right(True(file, start))
          case Identifier("false", file, start) => Right(False(file, start))

          case p @ Identifier("if", _, _)   => parseCond(p, toks)
          case p @ Identifier("val", _, _)  => parseVal(p, toks)
          case p @ Identifier("func", _, _) => parseFunc(p, toks)
          case p @ Identifier("let", _, _)  => parseLet(p, toks)

          case id: Identifier =>
            peek(toks) match {
              case Some(_: OpenParen) => parseApp(id, toks)
              case _                  => Right(id)
            }

          case ok: Binding => Right(ok)
          case ok: Cond    => Right(ok)
          case ok: Let     => Right(ok)
          case ok: Scalar  => Right(ok)

          case EOF(file, pos)       => Left(UnexpectedEOF(file, pos))
          case err: CloseParen      => Left(InvalidExpr(err))
          case err: OpenParen       => Left(InvalidExpr(err))
          case err: Comma           => Left(InvalidExpr(err))
          case err: Colon           => Left(InvalidExpr(err))
          case err: Eq              => Left(InvalidExpr(err))
          case err: UnknownToken    => Left(InvalidExpr(err))
          case err: UnexpectedToken => Left(InvalidExpr(err))
        }

  def parseLet(
      start: Token,
      toks: BufferedIterator[Token]
  ): Either[Error, Let] =
    for {
      vals <- parseBindings(start, toks).right
      key1 <- expect(if (vals.isEmpty) start else vals.last, wordIn, toks).right
      body <- next(key1, toks).right
    } yield Let(vals, body, start.getStart)

  def parseVal(
      start: Positioned,
      toks: BufferedIterator[Token]
  ): Either[Error, Binding] =
    for {
      name <- expect[Identifier](start, toks).right
      typ <- parseOptionalType(toks, name).right
      sign <- expect[Eq](typ._2, toks).right
      body <- next(sign, toks).right
    } yield Binding(Variable(name, typ._1), body, start.getStart)

  def parseFunc(
      start: Positioned,
      toks: BufferedIterator[Token]
  ): Either[Error, Binding] =
    for {
      name <- expect[Identifier](start, toks).right
      opar <- expect[OpenParen](name, toks).right
      args <- parseCommaSeparated(opar, charCloseParen, toks)(next).right
      cpar <- expect[CloseParen](args.lastOption.getOrElse(opar), toks).right
      sign <- expect[Eq](cpar, toks).right
      body <- next(sign, toks).right
    } yield Binding(Function(name, args, cpar), body, start.getStart)

  def parseBinding(
      tok: Token,
      toks: BufferedIterator[Token]
  ): Either[Error, Binding] = tok match {
    case Identifier("val", _, _)  => parseVal(tok, toks)
    case Identifier("func", _, _) => parseFunc(tok, toks)
    case _ =>
      Left(UnexpectedExpr(tok, "expecting a val or a func declaration"))
  }

  def parseBindings(
      start: Positioned,
      toks: BufferedIterator[Token]
  ): Either[Error, List[Binding]] =
    peek(toks) match {
      case None                                  => Right(Nil)
      case Some(word) if Token.eqv(word, wordIn) => Right(Nil)
      case _ =>
        eat(start, toks).flatMap { tok =>
          parseBinding(tok, toks).flatMap { h =>
            parseBindings(Positioned.at(h), toks).flatMap { t =>
              Right(h :: t)
            }
          }
        }
    }

  def parseOptionalType(
      toks: BufferedIterator[Token],
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

  def parseCond(start: Token, toks: BufferedIterator[Token]): Either[Error, Cond] =
    for {
      cond <- next(start, toks).right
      key1 <- expect(cond, wordThen, toks).right
      pass <- next(key1, toks).right
      key2 <- expect(pass, wordElse, toks).right
      fail <- next(key2, toks).right
    } yield Cond(cond, pass, fail, start.getStart)

  def parseApp(
      id: Identifier,
      toks: BufferedIterator[Token]
  ): Either[Error, App] =
    for {
      opar <- expect[OpenParen](id, toks).right
      args <- parseCommaSeparated(opar, charCloseParen, toks)(next).right
      cpar <- expect[CloseParen](args.lastOption.getOrElse(opar), toks).right
    } yield App(id, args, cpar)

  def parseCommaSeparated[T: ClassTag](
      start: Positioned,
      closer: Token,
      toks: BufferedIterator[Token]
  )(
      parseFn: (Positioned, BufferedIterator[Token]) => Either[Error, T] =
        next _
  )(implicit conv: T => Expr): Either[Error, List[T]] =
    peek(toks) match {
      case None                                  => Right(Nil)
      case Some(word) if Token.eqv(word, closer) => Right(Nil)
      case _ =>
        parseFn(start, toks).flatMap { h =>
          peek(toks) match {
            case None => Right(Nil)
            case Some(word) if Token.eqv(word, closer) =>
              Right(List(h))

            case Some(_) =>
              peek(toks) match {
                case Some(comma) if Token.eqv(comma, charComma) =>
                  eat(h, toks)
                  parseCommaSeparated(comma, closer, toks)(parseFn).flatMap {
                    t =>
                      Right(h :: t)
                  }

                case Some(t) if classTag[T].runtimeClass.isInstance(t) =>
                  Right(List(h, t.asInstanceOf[T]))

                case _ => Right(Nil)
              }
          }
        }
    }

  /** Peeks at the next token, if any, without moving forward.
    */
  def peek(toks: BufferedIterator[Token]): Option[Token] =
    toks.headOption

  /** Safely returns the next token from the tokens buffer. If the buffer is
    * empty an EOF error is returned.
    */
  def eat(last: Positioned, toks: BufferedIterator[Token]): Either[Error, Token] =
    if (!toks.hasNext)
      Left(UnexpectedEOF(last.getFile, last.getEnd))
    else
      toks.next match {
        case EOF(file, pos) => Left(UnexpectedEOF(file, pos))
        case next           => Right(next)
      }

  /** Safely returns the next parsable expression. A [[Left[Error]] is returned
    * if the token buffer is empty ro when an [[Error]] propagates from parsing
    * the next expression.
    */
  def next(last: Positioned, toks: BufferedIterator[Token]): Either[Error, Expr] =
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
      toks: BufferedIterator[Token]
  ): Either[Error, Token] =
    eat(last, toks).flatMap { tok =>
      tok match {
        case got: Error => Left(got)
        case got if !Token.eqv(got, expecting) =>
          Left(InvalidExpr(got, Some(expecting)))
        case got => Right(got)
      }
    }

  /** Overloaded [[expect]] with type check instead of equivalence check.
    */
  def expect[Expecting: ClassTag](
      last: Positioned,
      toks: BufferedIterator[Token]
  ): Either[Error, Expecting] =
    eat(last, toks).flatMap { tok =>
      tok match {
        case got: Error => Left(got)
        case got if classTag[Expecting].runtimeClass.isInstance(got) =>
          Right(got.asInstanceOf[Expecting])
        case got => Left(InvalidExpr(got))
      }
    }
}
