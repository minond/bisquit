package bisquit
package runtime

import scala.language.implicitConversions
import scala.reflect.ClassTag

import ast._
import scope._
import utils.{ensure, remap}
import utils.Implicits.Eithers

sealed trait RuntimeError
case class LookupError(id: Id) extends RuntimeError
case class ArgumentTypeError(arg: IR) extends RuntimeError
case class ConditionError(cond: IR) extends RuntimeError


def pass1(expr: Expression): IR with Expression =
  expr match {
    case ir: (Id | Int | Str | Bool) => ir
    case Uniop(op, a) => App(op, List(pass1(a)))
    case Binop(op, l, r) => App(op, List(pass1(l), pass1(r)))
    case Lambda(params, body, scope) => Lambda(params, pass1(body), scope)
    case App(fn, args) => App(fn, args.map(pass1))
    case Cond(cond, pass, fail) => Cond(pass1(cond), pass1(pass), pass1(fail))
    case Let(bindings, body) => Let(remap(bindings) { pass1 }, pass1(body))
  }


def eval(exprs: List[IR]): Either[RuntimeError, List[Value]] =
  eval(exprs, Map())

def eval(expr: IR): Either[RuntimeError, Value] =
  eval(expr, Map())

def eval(exprs: List[IR], scope: RuntimeScope): Either[RuntimeError, List[Value]] =
  exprs.map { eval(_, scope) }.squished()

def eval(expr: IR, scope: RuntimeScope): Either[RuntimeError, Value] =
  expr match {
    case Lambda(args, body, _) => Right(Lambda(args, body, scope))
    case value: Value => Right(value)
    case id: Id => lookup(id, scope)
    case App(fn, args) => evalCallable(pass1(fn), args.map(pass1), scope)
    case Let(bindings, body) =>
      for
        bound <- letRec(bindings, scope)
        ret <- eval(pass1(body), bound)
      yield ret
    case Cond(cond, pass, fail) =>
      for
        res <- eval(pass1(cond), scope)
        bool <- ensure[RuntimeError, Bool](res, ConditionError(pass1(cond)))
        body = if bool.value then pass else fail
        ret <- eval(pass1(body), scope)
      yield ret
  }

def evalCallable(fn: IR, args: List[IR], scope: RuntimeScope) =
  for
    maybeCallable <- eval(fn, scope)
    callable <- ensure[RuntimeError, Callable](maybeCallable, ArgumentTypeError(fn))
    ret <- callable.apply(args, scope)
  yield ret


def letRec(bindings: Map[String, Expression], scope: RuntimeScope) =
  bindings.foldLeft[Either[RuntimeError, RuntimeScope]](Right(scope)) {
    case (acc, (label, expr)) =>
      acc.flatMap { recscope =>
        eval(pass1(expr), recscope).map { v =>
          recscope ++ Map(label -> v)
        }
      }
  }


def lookup(id: Id, scope: RuntimeScope): Either[LookupError, Value] =
  scope.get(id.lexeme) match {
    case None => Left(LookupError(id))
    case Some(value) => Right(value)
  }
