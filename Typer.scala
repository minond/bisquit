package xyz.minond.bisquit.typer

import xyz.minond.bisquit.ast._
import xyz.minond.bisquit.scope._
import xyz.minond.bisquit.runtime._
import xyz.minond.bisquit.utils.ensure

sealed trait Type
case object UnitType extends Type
case object NumType extends Type
case object BoolType extends Type

case class FuncType(tys: Type*) extends Type {
  def apply(args: Type*): Type =
    apply(args.toList)

  def apply(args: List[Type]): Type =
    FuncType(tys.drop(args.size): _*).flatten

  def flatten =
    if tys.size == 1
    then tys.head
    else this
}

sealed trait TypingError
case class LookupError(id: Id) extends TypingError
case class CondMismatchError(cond: Cond, pass: Type, fail: Type) extends TypingError

type Signature = FuncType

def deduce(expr: Expression): Either[TypingError, Type] =
  deduce(expr, Map())

def deduce(expr: Expression, scope: TypeScope): Either[TypingError, Type] =
  expr match {
    case _: Num => Right(NumType)
    case _: Bool => Right(BoolType)
    case Builtin(sig, _) => Right(sig)
    case Binop(op, left, right) => deduceBinop(op, left, right, scope)
    case cond @ Cond(_, passExpr, failExpr) => deduceCond(cond, scope)
    case let @ Let(bindings, body) =>
      ???
  }

def deduceBinop(op: Id, left: Expression, right: Expression, scope: TypeScope) =
  for
    maybeFunc <- lookup(op, scope)
    opTy <- ensure[TypingError, FuncType](maybeFunc, LookupError(op))
    leftTy <- deduce(left, scope)
    rightTy <- deduce(right, scope)
  yield opTy.apply(leftTy, rightTy)

def deduceCond(cond: Cond, scope: TypeScope) =
  def branchesAreOfEqualType(pass: Type, fail: Type) =
    if pass == fail
    then Right(None)
    else Left(CondMismatchError(cond, pass, fail))
  for
    pass <- deduce(cond.pass, scope)
    fail <- deduce(cond.fail, scope)
    _ <- branchesAreOfEqualType(pass, fail)
  yield pass

def bound(bindings: Map[String, Expression], scope: TypeScope) =
  bindings.foldLeft[Either[TypingError, TypeScope]](Right(scope)) {
    case (acc, (label, expr)) =>
      acc.flatMap { recscope =>
        deduce(expr, recscope).map { v =>
          recscope ++ Map(label -> v)
        }
      }
  }

def lookup(id: Id, scope: TypeScope): Either[TypingError, Type] =
  scope.get(id.lexeme) match {
    case None => Left(LookupError(id))
    case Some(value) => Right(value)
  }
