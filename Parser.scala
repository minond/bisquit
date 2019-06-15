package xyz.minond.bisquit

import scala.reflect.{classTag, ClassTag}

object Parser {
  val wordThen = Identifier("then", "<internal>", 0)
  val wordElse = Identifier("else", "<internal>", 0)
  val wordIn = Identifier("in", "<internal>", 0)

  val charComma = Comma("<internal>", 0)
  val charCloseParen = CloseParen("<internal>", 0)

  /** Allows parseAnnotatedVarWithoutTokenContinuations to be used as a parsing
    * function.
    */
  implicit def var2expr(v: Variable): Expr = v

  def process(str: String, file: String): Iterator[Either[Error, Expr]] =
    parse(lex(str, file).buffered)

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

          case ok: App      => Right(ok)
          case ok: Binding  => Right(ok)
          case ok: Cond     => Right(ok)
          case ok: Function => Right(ok)
          case ok: Let      => Right(ok)
          case ok: Scalar   => Right(ok)

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
      vari <- parseAnnotatedVar(start, toks).right
      sign <- expect[Eq](vari._2, toks).right
      body <- next(sign, toks).right
    } yield Binding(vari._1, body, start.getStart)

  def parseAnnotatedVar(
      start: Positioned,
      toks: BufferedIterator[Token]
  ): Either[Error, (Variable, Token)] =
    for {
      name <- expect[Identifier](start, toks).right
      typ <- parseOptionalType(toks, name).right
    } yield (Variable(name, typ._1), typ._2)

  /** Helper for parsing `<var> [ ":" <ann> ]` expressions with a signature
    * that allows the function to be used as a parser combinator. The
    * unfortunate thing about doing this though is that we throw out the
    * "previously parsed" token used in error messages. This could lead to
    * parsing/type errors that don't point to the exact position of the error.
    */
  def parseAnnotatedVarWithoutTokenContinuations(
      start: Positioned,
      toks: BufferedIterator[Token]
  ): Either[Error, Variable] =
    for {
      vari <- parseAnnotatedVar(start, toks).right
    } yield vari._1

  def parseFunc(
      start: Positioned,
      toks: BufferedIterator[Token]
  ): Either[Error, Binding] = {
    val name =
      if (peekIs[Identifier](toks))
        Some(toks.next.asInstanceOf[Identifier])
      else
        None

    for {
      opar <- expect[OpenParen](name.getOrElse(start), toks).right
      args <- parseCommaSeparated(opar, charCloseParen, toks)(
        parseAnnotatedVarWithoutTokenContinuations
      ).right
      cpar <- expect[CloseParen](args.lastOption.getOrElse(opar), toks).right
      rtyp <- parseOptionalType(toks, cpar).right
      sign <- expect[Eq](rtyp._2, toks).right
      body <- next(sign, toks).right
    } yield Binding(Function(args, name, rtyp._1, sign), body, start.getStart)
  }

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

  def parseCond(
      start: Token,
      toks: BufferedIterator[Token]
  ): Either[Error, Cond] =
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
            case None                                  => Right(Nil)
            case Some(word) if Token.eqv(word, closer) => Right(List(h))

            case Some(_) =>
              peek(toks) match {
                case Some(comma) if Token.eqv(comma, charComma) =>
                  // XXX Why is eating here not moving ahead? eat(h, toks)
                  toks.next
                  parseCommaSeparated[T](comma, closer, toks)(parseFn).flatMap {
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

  /** Peeks at the next token without moving forward and returns true if it is
    * of type `Expecting`.
    */
  def peekIs[Expecting: ClassTag](toks: BufferedIterator[Token]): Boolean =
    peek(toks) match {
      case Some(next) if classTag[Expecting].runtimeClass.isInstance(next) =>
        true
      case _ => false
    }

  /** Safely returns the next token from the tokens buffer. If the buffer is
    * empty an EOF error is returned.
    */
  def eat(
      last: Positioned,
      toks: BufferedIterator[Token]
  ): Either[Error, Token] =
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
  def next(
      last: Positioned,
      toks: BufferedIterator[Token]
  ): Either[Error, Expr] =
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

  type Predicate[T] = T => Boolean

  def lex(str: String, file: String): Iterator[Token] = {
    val src = str.toList.toIterator.zipWithIndex.buffered
    for ((c, pos) <- src if !c.isWhitespace)
      yield
        c match {
          case '(' => OpenParen(file, pos)
          case ')' => CloseParen(file, pos)
          case ',' => Comma(file, pos)
          case ':' => Colon(file, pos)
          case '=' => Eq(file, pos)

          case '"' => lexStr('"', src, file, pos)
          case '`' => lexStr('`', src, file, pos)

          case n
              if isDigit(n) || (is('-')(n) &&
                src.hasNext &&
                isDigit(src.head._1)) =>
            val num = n + consumeWhile(src, isNumeric).mkString
            Num(num, deriveNumKind(num), file, pos)

          case c if isLetter(c) =>
            Identifier(
              c + consumeWhile(src, isIdentifierTail).mkString,
              file,
              pos
            )

          case c =>
            UnknownToken(c + consumeWhile(src, isWord).mkString, file, pos)
        }
  }

  def lexStr(
      end: Char,
      src: BufferedIterator[(Char, Int)],
      file: String,
      pos: Int
  ): Token = {
    val str = consumeWhile(src, not(is(end))).mkString
    if (!src.hasNext)
      EOF(file, str.size + pos)
    else
      src.next match {
        case (_end, _) if _end == end => Str(str, file, pos)
        case (bad, _) =>
          UnexpectedToken(
            bad.toString,
            s"expecting ${end} at end of string",
            file,
            pos
          )
      }
  }

  // XXX This needs some work...
  def deriveNumKind(str: String): NumKind =
    if (str.contains("0x")) Hex
    else if (str.contains("0b")) Bin
    else if (str.contains(".")) Real
    else Int

  def consumeWhile[T](
      src: BufferedIterator[(T, Int)],
      predicate: Predicate[T]
  ): Iterator[T] = {
    def aux(buff: List[T]): List[T] =
      if (src.hasNext && predicate(src.head._1)) {
        val curr = src.head._1
        src.next
        aux(buff :+ curr)
      } else buff

    aux(List.empty).toIterator
  }

  def isIdentifierTail(c: Char): Boolean =
    isLetter(c) || isDigit(c) || c == '_'

  def isNumeric(c: Char): Boolean =
    isDigit(c) || c == 'x' || c == 'b' || c == '.'

  def isDigit(c: Char): Boolean =
    c >= '0' && c <= '9'

  def isLetter(c: Char): Boolean =
    (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

  def isParen(c: Char): Boolean =
    c == '(' || c == ')'

  def isWord(c: Char): Boolean =
    !c.isWhitespace && !isParen(c)

  def is[T](x: T): Predicate[T] =
    (y: T) => x == y

  def not[T](f: Predicate[T]): Predicate[T] =
    (x: T) => !f(x)
}
