package bisquit
package parser

import nodes.{Int => _, _}
import errors._
import input.{Position, Positioned, Positioner}
import utils.Implicits.{Lists, Iterators}
import utils.ensure

import scala.util.{Try, Success, Failure}
import scala.reflect.ClassTag


type Source = BufferedIterator[(Char, Int)]
type Tokens = BufferedIterator[Token]


object Word {
  val Let = Id("let")
  val In = Id("in")
  val If = Id("if")
  val Then = Id("then")
  val Else = Id("else")
  val Fn = Id("fn")

  val True = Id("true")
  val False = Id("false")

  val Def = Id("def")
  val Import = Id("import")
  val Module = Id("module")
  val Exposing = Id("exposing")

  def isKeyword(token: Token) =
       token != Let
    && token != In
    && token != If
    && token != Then
    && token != Else
    && token != Fn
}


def parse(string: String, fileName: String): Iterator[Either[ParsingError, Expression | Statement]] =
  lex(string.trim, fileName).squished() match {
    case Left(err) => List(Left(err)).iterator
    case Right(tokens) => parse(tokens.iterator.buffered)
  }

def parse(tokens: Tokens): Iterator[Either[ParsingError, Expression | Statement]] =
  for
    token <- tokens
  yield
    parseTopLevel(token, tokens)

def parseTopLevel(token: Token, tokens: Tokens): Either[ParsingError, Expression | Statement] =
  token match {
    case Word.Def | Word.Import | Word.Module => parseStatement(token, tokens)
    case _ => parseExpression(token, tokens)
  }

def parseStatement(token: Token, tokens: Tokens): Either[ParsingError, Statement] =
  token match {
    case Word.Def => parseDefinition(tokens)
    case Word.Import => parseImport(tokens)
    case Word.Module => parseModule(tokens)
  }

def parseModule(tokens: Tokens): Either[ParsingError, Module] =
  (eat[Id](tokens), lookahead(tokens)) match {
    case (Left(err), _) => Left(err)

    case (Right(name), Word.Exposing) =>
      for
        _ <- eat[OpenParen](drop1(tokens))
        maybeExposing <- parseByUntil(tokens, Comma(), CloseParen())
        exposing <- maybeExposing.ensureItems[ParsingError, Id]({ err => UnexpectedExpression[Id](err) })
      yield
        Module(name, exposing.toSet, Map())

    case (Right(name), _) => Right(Module(name, Set.empty, Map()))
  }

def parseImport(tokens: Tokens): Either[ParsingError, Import] =
  (eat[Id](tokens), lookahead(tokens)) match {
    case (Left(err), _) => Left(err)

    case (Right(name), Word.Exposing) =>
      eat[OpenParen](drop1(tokens)).flatMap { _ =>
        lookahead(tokens) match {
          case _: Dot =>
            for
              _ <- eat[Dot](tokens)
              _ <- eat[Dot](tokens)
              _ <- eat[Dot](tokens)
              _ <- eat[CloseParen](tokens)
            yield
              Import(name, List.empty, true)

          case _ =>
            parseByUntil(tokens, Comma(), CloseParen()).flatMap { maybeExposing =>
              maybeExposing.ensureItems[ParsingError, Id]({ err => UnexpectedExpression[Id](err) })
                .map { Import(name, _) }
            }
        }
      }

    case (Right(name), _) => Right(Import(name, List.empty))
  }

def parseDefinition(tokens: Tokens): Either[ParsingError, Definition] =
  (eat[Id](tokens), lookahead(tokens)) match {
    case (Left(err), _) => Left(err)

    case (Right(name), Equal()) =>
      for
        value <- parseExpression(drop1(tokens))
      yield
        Definition(name, value)

    case (Right(name), OpenParen()) =>
      for
        value <- parseLambda(tokens)
      yield
        Definition(name, value)

    case (Right(name), invalid) =>
      Left(UnexpectedToken[Equal | OpenParen](invalid))
  }

def parseExpression(tokens: Tokens): Either[ParsingError, Expression] =
  if tokens.isEmpty
  then Left(UnexpectedEOF())
  else parseExpression(tokens.next, tokens)

def parseExpression(token: Token, tokens: Tokens): Either[ParsingError, Expression] =
  token match {
    case Word.True => Right(Bool(true))
    case Word.False => Right(Bool(false))
    case Word.Let => parseLet(tokens)
    case Word.Fn => parseExpressionContinuation(parseLambda(tokens), tokens)
    case Word.If => parseCond(tokens)
    case OpenParen() => parseExpressionContinuation(parseTuple(tokens), tokens)
    case OpenCurlyBraket() => parseExpressionContinuation(parseRecord(tokens), tokens)
    case OpenSquareBraket() => parseLista(tokens)
    case scalar: (Str | nodes.Int) => Right(scalar)
    case id: Id => parseExpressionContinuation(id, tokens)
    case unexpected => Left(UnexpectedToken[Token](unexpected))
  }

def parseLet(tokens: Tokens): Either[ParsingError, Let] =
  for
    bindings <- parseBindings(tokens)
    _ <- eat(Word.In, tokens)
    body <- parseExpression(tokens)
  yield
    Let(bindings.toMap, body)

def parseRecordLookup(rec: Expression, tokens: Tokens): Either[ParsingError, RecordLookup] =
  for
    _ <- eat(Dot(), tokens)
    field <- eat[Id](tokens)
  yield
    RecordLookup(rec, field)

def parseLista(tokens: Tokens): Either[ParsingError, Expression] =
  for
    items <- parseByUntil(tokens, Comma(), CloseSquareBraket())
  yield
    Lista(items)

def parseTuple(tokens: Tokens): Either[ParsingError, Expression] =
  for
    fields <- parseByUntil(tokens, Comma(), CloseParen())
  yield
    if fields.size == 1
    then fields.head
    else Tuple(fields)

def parseRecord(tokens: Tokens): Either[ParsingError, Record] =
  for
    fields <- parseFields(tokens)
    _ <- eat(CloseCurlyBraket(), tokens)
  yield
    Record(fields.toMap)

def parseFields(tokens: Tokens): Either[ParsingError, List[(Id, Expression)]] =
  lookahead(tokens) match {
      case next if next == CloseCurlyBraket() => Right(List.empty)
      case _ =>
        for
          binding <- parseBinding(tokens)
        yield
          lookahead(tokens) match {
            case next if next == CloseCurlyBraket() => List(binding)
            case next if next == Comma() =>
              tokens.next
              parseFields(tokens) match {
                case Left(err) => return Left(err)
                case Right(bindings) => binding +: bindings
              }
          }
  }

def parseBindings(tokens: Tokens): Either[ParsingError, List[(Id, Expression)]] =
  for
    binding <- parseBinding(tokens)
  yield
    lookahead(tokens) match {
      case next if next == Word.In => List(binding)
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

def parseExpressionContinuation(headRes: Either[ParsingError, Expression], tokens: Tokens): Either[ParsingError, Expression] =
  headRes.flatMap { head => parseExpressionContinuation(head, tokens) }

def parseExpressionContinuation(head: Expression, tokens: Tokens): Either[ParsingError, Expression] =
  lookahead(tokens) match {
    case OpenParen() => parseExpressionContinuation(parseApp(head, tokens), tokens)
    case Dot() => parseExpressionContinuation(parseRecordLookup(head, tokens), tokens)
    case _ => Right(head)
  }

def parseCond(tokens: Tokens): Either[ParsingError, Cond] =
  for
    cond <- parseExpression(tokens)
    _ <- eat(Word.Then, tokens)
    pass <- parseExpression(tokens)
    _ <- eat(Word.Else, tokens)
    fail <- parseExpression(tokens)
  yield
    Cond(cond, pass, fail)

def parseLambda(tokens: Tokens): Either[ParsingError, Lambda] =
  for
    _ <- eat(OpenParen(), tokens)
    maybeParams <- parseByUntil(tokens, Comma(), CloseParen())
    params <- maybeParams.ensureItems[ParsingError, Id]({ err => UnexpectedExpression[Id](err) })
    _ <- eat(Equal(), tokens)
    body <- parseExpression(tokens)
  yield
    Lambda(params, body, None)

def parseApp(callee: Expression, tokens: Tokens): Either[ParsingError, App] =
  for
    _ <- eat(OpenParen(), tokens)
    args <- parseByUntil(tokens, Comma(), CloseParen())
  yield
    App(callee, args)

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


def drop1(tokens: Tokens): Tokens =
  tokens.next
  tokens

def next(tokens: Tokens): Either[ParsingError, Token] =
  if tokens.isEmpty
  then Left(UnexpectedEOF())
  else Right(tokens.next)

def eat[T: ClassTag](tokens: Tokens): Either[ParsingError, T] =
  if tokens.isEmpty
  then Left(UnexpectedEOF())
  else
    val next = tokens.next
    ensure[ParsingError, T](next, UnexpectedToken[T](next))

def eat[T: ClassTag](expected: T, tokens: Tokens): Either[ParsingError, T] =
  if tokens.isEmpty
  then Left(UnexpectedEOF())
  else
    val next = tokens.next
    if next == expected
    then Right(next.asInstanceOf[T])
    else Left(UnexpectedToken[T](next))

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
    case '{' => ok(OpenCurlyBraket())
    case '}' => ok(CloseCurlyBraket())
    case '[' => ok(OpenSquareBraket())
    case ']' => ok(CloseSquareBraket())
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
        case Success(i) => ok(nodes.Int(i))
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
                           not(is('{')),
                           not(is('}')),
                           not(is('[')),
                           not(is(']')),
                           not(is(',')),
                           not(is('.')),
                           not(is('=')),
                           not(is(':')))
val isIdentifierHead = and(isIdentifierTail,
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
