package bisquit
package parser

import ast.{Int => _, _}
import input.{Position, Positioned, Positioner}
import utils.ensure
import utils.Implicits.{Lists, Iterators}

import scala.util.{Try, Success, Failure}
import scala.reflect.ClassTag


type Source = BufferedIterator[(Char, Int)]
type Tokens = BufferedIterator[Token]


trait ParsingError extends Positioned
case class UnexpectedToken[Expected: ClassTag]() extends ParsingError
case class UnexpectedEOF() extends ParsingError
case class StringNotClosed() extends ParsingError
case class InvalidInteger(lexeme: String) extends ParsingError
case class InvalidCharacter(c: Char) extends ParsingError


object Keywords {
  val Let = Id("let")
  val In = Id("in")
  val If = Id("if")
  val Then = Id("then")
  val Else = Id("else")
  val Fn = Id("fn")

  def isKeyword(token: Token) =
       token != Let
    && token != In
    && token != If
    && token != Then
    && token != Else
    && token != Fn
}


def parse(string: String, fileName: String): Iterator[Either[ParsingError, Expression]] =
  lex(string.trim, fileName).squished() match {
    case Left(err) => List(Left(err)).iterator
    case Right(tokens) => parse(tokens.iterator.buffered)
  }

def parse(tokens: Tokens): Iterator[Either[ParsingError, Expression]] =
  for
    token <- tokens
  yield
    parseExpression(token, tokens)

def parseLet(tokens: Tokens): Either[ParsingError, Let] =
  for
    bindings <- parseBindings(tokens)
    _ <- eat(Keywords.In, tokens)
    body <- parseExpression(tokens)
  yield
    Let(bindings.toMap, body)

def parseBindings(tokens: Tokens): Either[ParsingError, List[(Id, Expression)]] =
  for
    binding <- parseBinding(tokens)
  yield
    lookahead(tokens) match {
      case next if next == Keywords.In => List(binding)
      case next =>
        parseBindings(tokens) match {
          case Left(err) => return Left(err)
          case Right(bindings) => binding +: bindings
        }
    }

def parseBinding(tokens: Tokens): Either[ParsingError, (Id, Expression)] =
  for
    name <- eat[Id](tokens)
    _ <- eat[Equal](tokens)
    value <- parseExpression(tokens)
  yield
    (name, value)

def parseExpression(tokens: Tokens): Either[ParsingError, Expression] =
  if tokens.isEmpty
  then Left(UnexpectedEOF())
  else parseExpression(tokens.next, tokens)

def parseExpression(token: Token, tokens: Tokens): Either[ParsingError, Expression] =
  token match {
    case Keywords.Let => parseLet(tokens)
    case Keywords.Fn => parseLambda(tokens)
    case Keywords.If => parseCond(tokens)
    case Id("true") => Right(Bool(true))
    case Id("false") => Right(Bool(false))
    case str: Str => Right(str)
    case int: ast.Int => Right(int)
    case id: Id =>
      if lookahead(tokens) == OpenParen()
      then parseApp(id, tokens)
      else Right(id)
  }

def parseCond(tokens: Tokens): Either[ParsingError, Cond] =
  for
    cond <- parseExpression(tokens)
    _ <- eat(Keywords.Then, tokens)
    pass <- parseExpression(tokens)
    _ <- eat(Keywords.Else, tokens)
    fail <- parseExpression(tokens)
  yield
    Cond(cond, pass, fail)

def parseLambda(tokens: Tokens): Either[ParsingError, Lambda] =
  for
    _ <- eat(OpenParen(), tokens)
    maybeParams <- parseByUntil(tokens, Comma(), CloseParen())
    params <- maybeParams.ensureItems[ParsingError, Id](UnexpectedToken[Id]())
    _ <- eat(Equal(), tokens)
    body <- parseExpression(tokens)
  yield
    Lambda(params, body)

def parseApp(id: Id, tokens: Tokens): Either[ParsingError, App] =
  for
    _ <- eat(OpenParen(), tokens)
    args <- parseByUntil(tokens, Comma(), CloseParen())
  yield
    App(id, args)

def parseByUntil(
    tokens: Tokens,
    sep: Token,
    until: Token,
    acc: List[Expression] = List.empty,
): Either[ParsingError, List[Expression]] =
  next(tokens) match {
    case Left(err) => Left(err)
    case Right(token) =>
      if token == until
      then Right(acc)
      else if token == sep
      then parseByUntil(tokens, sep, until, acc)
      else parseExpression(token, tokens).flatMap { expr =>
        parseByUntil(tokens, sep, until, acc :+ expr)
      }
  }


def next(tokens: Tokens): Either[ParsingError, Token] =
  if tokens.isEmpty
  then Left(UnexpectedEOF())
  else Right(tokens.next)

def eat[T: ClassTag](tokens: Tokens): Either[ParsingError, T] =
  if tokens.isEmpty
  then Left(UnexpectedEOF())
  else ensure[ParsingError, T](tokens.next, UnexpectedToken[T]())

def eat[T: ClassTag](expected: T, tokens: Tokens): Either[ParsingError, T] =
  if tokens.isEmpty
  then Left(UnexpectedEOF())
  else
    val next = tokens.next
    if next == expected
    then Right(next.asInstanceOf[T])
    else Left(UnexpectedToken[T]())

def lookahead(tokens: Tokens): Token =
  if tokens.hasNext
  then tokens.head
  else Eof()


def lex(string: String, fileName: String): Iterator[Either[ParsingError, Token]] =
  val source = string.iterator.zipWithIndex.buffered
  val positioner = Positioned.file(fileName)

  for
    (c, offset) <- source if !c.isWhitespace
  yield
    nextToken(c, source, positioner, offset)

def nextToken(
    char: Char,
    source: Source,
    positioner: Positioner,
    offset: Int,
): Either[ParsingError, Token] =
  def ok(token: Token) =
    Right(positioner(token, offset))
  def err(err: ParsingError) =
    Left(err.at(Position(positioner.file, offset)))
  char match {
    case '(' => ok(OpenParen())
    case ')' => ok(CloseParen())
    case ',' => ok(Comma())
    case ':' => ok(Colon())
    case '=' => ok(Equal())
    case '.' => ok(Dot())

    case '"' =>
      takeUntil(source, is('"')) match {
        case (_, NotFound) => err(StringNotClosed())
        case (chars, Found) => ok(Str(chars.mkString))
      }

    case n if isDigit(n) =>
      val str = (n +: takeWhile(source, isDigit)).mkString
      Try { str.toInt } match {
        case Failure(_) => err(InvalidInteger(str))
        case Success(i) => ok(ast.Int(i))
      }

    case x if isIdentifierHead(x) =>
      val str = (x +: takeWhile(source, isIdentifierTail)).mkString
      ok(Id(str))

    case invalid => err(InvalidCharacter(invalid))
  }


trait LookaheadOutcome
case object Found extends LookaheadOutcome
case object NotFound extends LookaheadOutcome

type Predicate[T] = T => Boolean

def ge[T <: Char](x: T) =
  (c: T) =>
    c >= x

def le[T <: Char](x: T) =
  (c: T) =>
    c <= x

def is[T](x: T) =
  (c: T) =>
    x == c

def not[T](f: Predicate[T]) =
  (c: T) =>
    !f(c)

def and[T](fs: Predicate[T]*) =
  (c: T) =>
    fs.foldLeft(true) { (res, f) => res && f(c) }

val isWhitespace = (c: Char) => c.isWhitespace
val isDigit = and(ge('0'), le('9'))
val isIdentifierTail = and(not(isWhitespace),
                           not(is('(')),
                           not(is(')')),
                           not(is(',')),
                           not(is(':')))
val isIdentifierHead = and(isIdentifierTail,
                           not(is(':')),
                           not(is('=')),
                           not(is('.')),
                           not(isDigit))


def takeUntil(source: Source, pred: Predicate[Char]): (List[Char], LookaheadOutcome) =
  def aux(buff: List[Char]): (List[Char], LookaheadOutcome) =
    if source.isEmpty
    then (buff, NotFound)
    else
      val curr = source.next
      if pred(curr._1)
      then (buff, Found)
      else aux(buff :+ curr._1)
  aux(List.empty)

def takeWhile(source: Source, pred: Predicate[Char]): List[Char] =
  def aux(buff: List[Char]): List[Char] =
    if source.isEmpty
    then buff
    else
      val curr = source.head
      if pred(curr._1)
      then aux(buff :+ source.next._1)
      else buff
  aux(List.empty)
