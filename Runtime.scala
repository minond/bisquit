package xyz.minond.bisquit.runtime

import xyz.minond.bisquit.token._
import xyz.minond.bisquit.utils.{ensure, Eithers}

sealed trait RuntimeError
case class NotCallable(value: Value) extends RuntimeError
case class UnknownOperator(op: Id) extends RuntimeError
case class LookupError(label: Id) extends RuntimeError
case class ArityError(label: Id, expected: Integer, got: Integer) extends RuntimeError

object Runtime {
  import scala.language.implicitConversions

  import Eithers._

  type Scope = Map[String, Value]

  def eval(exprs: List[Expression]): Either[RuntimeError, List[Value]] =
    eval(exprs, Map())

  def eval(expr: Expression): Either[RuntimeError, Value] =
    eval(expr, Map())

  def eval(exprs: List[Expression], scope: Scope): Either[RuntimeError, List[Value]] =
    exprs.map { eval(_, scope) }.squished()

  def eval(expr: Expression, scope: Scope): Either[RuntimeError, Value] =
    expr match {
      case value: Value => Right(value)
      case id: Id => lookup(id, scope)
      case Binop(op, left, right) =>
        for {
          l <- eval(left, scope)
          r <- eval(right, scope)
          ret <- applyOp(op, Some(l), Some(r))
        } yield ret
      case Uniop(op, subject) =>
        for {
          arg <- eval(subject, scope)
          ret <- applyOp(op, Some(arg), None)
        } yield ret
      case App(fn, args) =>
        for {
          vals <- eval(args, scope)
          ret <- applyFunc(fn, vals, scope)
        } yield ret
    }

  def applyOp(op: Id, left: => Option[Value], right: => Option[Value]): Either[RuntimeError, Value] =
    (op.lexeme, left, right) match {
      // Binary number operation
      case ("+", Some(l: Num), Some(r: Num)) => Right(applyNumBinop(l, r) { _ + _ })
      case ("-", Some(l: Num), Some(r: Num)) => Right(applyNumBinop(l, r) { _ - _ })
      case ("*", Some(l: Num), Some(r: Num)) => Right(applyNumBinop(l, r) { _ * _ })
      case ("/", Some(l: Num), Some(r: Num)) => Right(applyNumBinop(l, r) { _ / _ })
      case ("%", Some(l: Num), Some(r: Num)) => Right(applyNumBinop(l, r) { _ % _ })

      // Uniary number operations
      case ("-", Some(n: Num), None) => Right(applyNumUniop(n) { -_ })
      case ("+", Some(n: Num), None) => Right(applyNumUniop(n) { Math.abs(_) })
      case ("!", Some(n: Num), None) => Right(applyNumUniop(n) { Math.exp(_) })

      case _ => Left(UnknownOperator(op))
    }

  def applyNumBinop(left: Num, right: Num)(f: (Double, Double) => Double): Num =
    Num(f(left.value, right.value))

  def applyNumUniop(right: Num)(f: Double => Double): Num =
    Num(f(right.value))

  def applyFunc(fn: Id, args: => List[Value], scope: Scope): Either[RuntimeError, Value] =
    def apply(func: Func) =
      eval(func.body, func.params.map(_.lexeme).zip(args).toMap ++ scope)

    def arityMatch(func: Func) =
      if (func.params.size != args.size)
        Left(ArityError(fn, func.params.size, args.size))
      else
        Right(())

    for {
      value <- lookup(fn, scope)
      func <- ensure[Func, RuntimeError, Value](value, NotCallable(value))
      _ <- arityMatch(func)
      ret <- apply(func)
    } yield ret

  def lookup(label: Id, scope: Scope): Either[LookupError, Value] =
    Right(scope.getOrElse(label.lexeme, return Left(LookupError(label))))
}
