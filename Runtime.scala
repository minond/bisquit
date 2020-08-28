package bisquit
package runtime

import scala.language.implicitConversions
import scala.reflect.ClassTag

import ast._
import scope._
import utils.ensure
import utils.Implicits.Eithers

sealed trait RuntimeError
case class LookupError(id: Id) extends RuntimeError
case class ArgumentTypeError(arg: Expression) extends RuntimeError
case class ConditionError(cond: Expression) extends RuntimeError


def eval(exprs: List[Expression]): Either[RuntimeError, List[Value]] =
  eval(exprs, Map())

def eval(expr: Expression): Either[RuntimeError, Value] =
  eval(expr, Map())

def eval(exprs: List[Expression], scope: RuntimeScope): Either[RuntimeError, List[Value]] =
  exprs.map { eval(_, scope) }.squished()

def eval(expr: Expression, scope: RuntimeScope): Either[RuntimeError, Value] =
  expr match {
    case Lambda(args, body, _) => Right(Lambda(args, body, scope))
    case value: Value => Right(value)
    case id: Id => lookup(id, scope)
    case Binop(op, left, right) => evalCallable(op, List(left, right), scope)
    case Uniop(op, subject) => evalCallable(op, List(subject), scope)
    case App(fn, args) => evalCallable(fn, args, scope)
    case Let(bindings, body) =>
      for
        bound <- letRec(bindings, scope)
        ret <- eval(body, bound)
      yield ret
    case Cond(cond, pass, fail) =>
      for
        res <- eval(cond, scope)
        bool <- ensure[RuntimeError, Bool](res, ConditionError(cond))
        body = if bool.value then pass else fail
        ret <- eval(body, scope)
      yield ret
  }

def evalCallable(fn: Expression, args: List[Expression], scope: RuntimeScope) =
  for
    maybeCallable <- eval(fn, scope)
    callable <- ensure[RuntimeError, Callable](maybeCallable, ArgumentTypeError(fn))
    ret <- callable.apply(args, scope)
  yield ret


def letRec(bindings: Map[String, Expression], scope: RuntimeScope) =
  bindings.foldLeft[Either[RuntimeError, RuntimeScope]](Right(scope)) {
    case (acc, (label, expr)) =>
      acc.flatMap { recscope =>
        eval(expr, recscope).map { v =>
          recscope ++ Map(label -> v)
        }
      }
  }


def lookup(id: Id, scope: RuntimeScope): Either[LookupError, Value] =
  scope.get(id.lexeme) match {
    case None => Left(LookupError(id))
    case Some(value) => Right(value)
  }
