package xyz.minond.bisquit.runtime

import scala.language.implicitConversions
import scala.reflect.ClassTag

import xyz.minond.bisquit.ast._
import xyz.minond.bisquit.utils.ensure
import xyz.minond.bisquit.utils.Implicits.Eithers

sealed trait RuntimeError
case class LookupError(id: Id) extends RuntimeError
case class ArgumentTypeError(arg: Expression) extends RuntimeError
case class ConditionError(cond: Expression) extends RuntimeError

type Scope = Map[String, Value]

def eval(exprs: List[Expression]): Either[RuntimeError, List[Value]] =
  eval(exprs, Map())

def eval(expr: Expression): Either[RuntimeError, Value] =
  eval(expr, Map())

def eval(exprs: List[Expression], scope: Scope): Either[RuntimeError, List[Value]] =
  exprs.map { eval(_, scope) }.squished()

def eval(expr: Expression, scope: Scope): Either[RuntimeError, Value] =
  expr match {
    case Func(args, body, _) => Right(Func(args, body, scope))
    case Cons(values) =>
      for
        vals <- eval(values, scope)
      yield Cons(vals)
    case value: Value => Right(value)
    case id: Id => lookup(id, scope)
    case Binop(op, left, right) => applyOp(op, List(left, right), scope)
    case Uniop(op, subject) => applyOp(op, List(subject), scope)
    case App(fn, args) =>
      for
        vals <- eval(args, scope)
        maybeFunc <- eval(fn, scope)
        func <- ensure[RuntimeError, Func](maybeFunc, ArgumentTypeError(fn))
        ret <- applyOrCurryFunc(func, vals)
      yield ret
    case Let(bindings, body) =>
      for
        bound <- letRec(bindings, scope)
        ret <- eval(body, bound)
      yield ret
    case Cond(cond, pass, fail) =>
      for
        res <- eval(cond, scope)
        bool <- ensure[RuntimeError, Bool](res, ConditionError(cond))
        body = if (bool.value) pass else fail
        ret <- eval(body, scope)
      yield ret
  }

def letRec(bindings: Map[String, Expression], scope: Scope): Either[RuntimeError, Scope] =
  bindings.foldLeft[Either[RuntimeError, Scope]](Right(scope)) {
    case (acc, (label, expr)) =>
      acc.flatMap { recscope =>
        eval(expr, recscope).map { v =>
          recscope ++ Map(label -> v)
        }
      }
  }

def applyOp(op: Id, args: => List[Expression], scope: Scope): Either[RuntimeError, Value] =
  lookup(op, scope).flatMap {
    case builtin: Builtin => builtin.apply(args, scope)
  }

def applyOrCurryFunc(func: Func, args: => List[Value]): Either[RuntimeError, Value] =
  if (func.params.size != args.size)
    Right(func.curried(args))
  else
    val argScope = func.params.map(_.lexeme).zip(args).toMap
    val lexScope = func.scope ++ argScope
    eval(func.body, lexScope)

def lookup(id: Id, scope: Scope): Either[LookupError, Value] =
  scope.get(id.lexeme) match {
    case None => Left(LookupError(id))
    case Some(value) => Right(value)
  }
