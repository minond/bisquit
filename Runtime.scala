package xyz.minond.bisquit.runtime

import xyz.minond.bisquit.token._

object Environment {
  type Scope = Map[String, Value]
}

sealed trait EvaluationError
case object ApplicationError extends EvaluationError
case object InvalidType extends EvaluationError
case class LookupError(label: String) extends EvaluationError
case class ArityError(label: String, expected: Integer, got: Integer) extends EvaluationError

object Eithers {
  implicit def listOfEithersToEithers[L, R](eithers: List[Either[L, R]]): Container[L, R] =
    Container(eithers)

  class Container[L, R](val eithers: List[Either[L, R]]) {
    def squished(): Either[L, List[R]] = {
      val acc: Either[L, List[R]] = Right(List())
      eithers.foldLeft(acc) {
        (acc, x) =>
          acc.right.flatMap(xs => x.right.map(_ +: xs))
      }
    }
  }
}

object Evaluator {
  import scala.language.implicitConversions

  import Environment._
  import Eithers._

  def eval(exprs: List[Expression], scope: Scope): Either[EvaluationError, List[Value]] =
    exprs.map { eval(_, scope) }.squished()

  def eval(expr: Expression, scope: Scope): Either[EvaluationError, Value] =
    expr match {
      case value: Value => Right(value)
      case Id(label) => lookup(label, scope)
      case Binop(op, left, right) =>
        for {
          l <- eval(left, scope).right
          r <- eval(right, scope).right
          ret <- applyOp(op, Some(l), Some(r)).right
        } yield ret
      case Uniop(op, subject) =>
        for {
          arg <- eval(subject, scope).right
          ret <- applyOp(op, Some(arg), None).right
        } yield ret
      case App(fn, args) =>
        for {
          vals <- eval(args, scope).right
          ret <- applyFunc(fn, vals, scope).right
        } yield ret

      case _ => ???
    }

  def applyOp(op: Id, left: => Option[Value], right: => Option[Value]): Either[EvaluationError, Value] =
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
    }

  def applyNumBinop(left: Num, right: Num)(f: (Double, Double) => Double): Num =
    Num(f(left.value, right.value))

  def applyNumUniop(right: Num)(f: Double => Double): Num =
    Num(f(right.value))

  def applyFunc(fn: Id, args: => List[Value], scope: Scope): Either[EvaluationError, Value] =
    def apply(func: Func) =
      eval(func.body, func.params.map(_.lexeme).zip(args).toMap ++ scope)

    def arityMatch(func: Func) =
      if (func.params.size != args.size)
        Left(ArityError(fn.lexeme, func.params.size, args.size))
      else
        Right(())

    for {
      value <- lookup(fn.lexeme, scope).right
      func <- ensure[Func](value).right
      _ <- arityMatch(func).right
      ret <- apply(func).right
    } yield ret

  def ensure[T](value: Value): Either[EvaluationError, T] =
    value match {
      case ok : T => Right(ok)
      case _ => Left(InvalidType)
    }

  def lookup(label: String, scope: Scope): Either[LookupError, Value] =
    Right(scope.getOrElse(label, return Left(LookupError(label))))
}
