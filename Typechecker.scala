package xyz.minond.bisquit.typechecker

import xyz.minond.bisquit.ast._
import xyz.minond.bisquit.scope._
import xyz.minond.bisquit.runtime._
import xyz.minond.bisquit.utils.ensure

trait Typing { self =>
  var ty: Option[Type] = None

  def typeTag(ty: Type): self.type =
    this.ty = Some(ty)
    this
}

sealed trait Type
case object UnitType extends Type
case object NumType extends Type
case object BoolType extends Type

case class FuncType(tys: List[Type]) extends Type {
  def apply(args: Type*): Type =
    apply(args.toList)

  def apply(args: List[Type]): Type =
    FuncType(tys.drop(args.size)).flatten

  def flatten =
    if tys.size == 1
    then tys.head
    else this
}

sealed trait TypingError
case class LookupError(id: Id) extends TypingError
case class CondMismatchError(cond: Cond, pass: Type, fail: Type) extends TypingError

def signature(tys: Type*) =
  FuncType(tys.toList)

def deduce(expr: Expression): Either[TypingError, Type] =
  deduce(expr, Map())

def deduce(expr: Expression, scope: RuntimeScope): Either[TypingError, Type] =
  expr match {
    case _: Num => Right(NumType)
    case _: Bool => Right(BoolType)
    case id : Id => lookup(id, scope)
    case Builtin(sig, _) => Right(sig)
    case Uniop(op, subject) => deduceUniop(op, subject, scope)
    case Binop(op, left, right) => deduceBinop(op, left, right, scope)
    case cond @ Cond(_, passExpr, failExpr) => deduceCond(cond, scope)
  }

def deduceUniop(op: Id, subject: Expression, scope: RuntimeScope) =
  for
    maybeFunc <- lookup(op, scope)
    opTy <- ensure[TypingError, FuncType](maybeFunc, LookupError(op))
    subjectTy <- deduce(subject, scope)
  yield opTy.apply(subjectTy)

def deduceBinop(op: Id, left: Expression, right: Expression, scope: RuntimeScope) =
  for
    maybeFunc <- lookup(op, scope)
    opTy <- ensure[TypingError, FuncType](maybeFunc, LookupError(op))
    leftTy <- deduce(left, scope)
    rightTy <- deduce(right, scope)
  yield opTy.apply(leftTy, rightTy)

def deduceCond(cond: Cond, scope: RuntimeScope) =
  def branchesAreOfEqualType(pass: Type, fail: Type) =
    if pass == fail
    then Right(None)
    else Left(CondMismatchError(cond, pass, fail))
  for
    pass <- deduce(cond.pass, scope)
    fail <- deduce(cond.fail, scope)
    _ <- branchesAreOfEqualType(pass, fail)
  yield pass

def lookup(id: Id, scope: RuntimeScope): Either[TypingError, Type] =
  scope.get(id.lexeme) match {
    case None => Left(LookupError(id))
    case Some(value) =>
      value.ty match {
        case None => Left(LookupError(id))
        case Some(ty) => Right(ty)
      }
  }
