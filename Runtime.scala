package xyz.minond.bisquit.runtime

import scala.language.implicitConversions

import xyz.minond.bisquit.token._
import xyz.minond.bisquit.utils.ensure
import xyz.minond.bisquit.utils.Implicits.Eithers

sealed trait RuntimeError
case class NotCallable(value: Value) extends RuntimeError
case class UnknownOperator(op: Id) extends RuntimeError
case class LookupError(label: Id) extends RuntimeError
case class ArityError(func: Id | Func, expected: Integer, got: Integer) extends RuntimeError


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
      for
        arg1 <- eval(left, scope)
        arg2 <- eval(right, scope)
        ret <- applyOp(op, List(arg1, arg2), scope)
      yield ret
    case Uniop(op, subject) =>
      for
        arg <- eval(subject, scope)
        ret <- applyOp(op, List(arg), scope)
      yield ret
    case App(fn, args) =>
      for
        vals <- eval(args, scope)
        ret <- applyFunc(fn, vals, scope)
      yield ret
  }

def applyOp(op: Id, args: => List[Value], scope: Scope): Either[RuntimeError, Value] =
  lookup(op, scope).map {
    case builtin: Builtin => builtin.apply(args)
  }

def applyFunc(fn: Id | Func, args: => List[Value], scope: Scope): Either[RuntimeError, Value] =
  def applyOrCurry(func: Func) =
    if (func.params.size != args.size)
      Right(func.curried(args))
    else
      val argScope = func.params.map(_.lexeme).zip(args).toMap
      val lexScope = argScope ++ scope
      eval(func.body, lexScope)

  def getFunc(): Either[RuntimeError, Func] =
    fn match {
      case fn: Func => Right(fn)
      case id: Id =>
        for
          value <- lookup(id, scope)
          func <- ensure[RuntimeError, Func](value, NotCallable(value))
        yield func
    }

  for
    func <- getFunc()
    ret <- applyOrCurry(func)
  yield ret

def lookup(label: Id, scope: Scope): Either[LookupError, Value] =
  Right(scope.getOrElse(label.lexeme, return Left(LookupError(label))))
