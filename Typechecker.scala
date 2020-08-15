package bisquit
package typechecker

import ast._
import scope._
import runtime._
import utils.ensure

trait Typing { self =>
  var ty: Option[Type] = None

  def typeTag(ty: Type): self.type =
    this.ty = Some(ty)
    this
}

sealed trait Type
case object UnitType extends Type
case object IntType extends Type
case object StrType extends Type
case object BoolType extends Type

case class LambdaType(tys: List[Type]) extends Type {
  def apply(args: Type*): Type =
    apply(args.toList)

  def apply(args: List[Type]): Type =
    LambdaType(tys.drop(args.size)).flatten

  def flatten =
    if tys.size == 1
    then tys.head
    else this
}

sealed trait TypingError
case class LookupError(id: Id) extends TypingError
case class CondMismatchError(cond: Cond, pass: Type, fail: Type) extends TypingError

def signature(tys: Type*) =
  LambdaType(tys.toList)

def deduce(expr: Expression): Either[TypingError, Type] =
  deduce(expr, Map())

def deduce(expr: Expression, env: Environment): Either[TypingError, Type] =
  expr match {
    case _: Int => Right(IntType)
    case _: Str => Right(StrType)
    case _: Bool => Right(BoolType)
    case id : Id => lookup(id, env)
    case Builtin(sig, _) => Right(sig)
    case Uniop(op, subject) => deduceUniop(op, subject, env)
    case Binop(op, left, right) => deduceBinop(op, left, right, env)
    case cond : Cond => deduceCond(cond, env)
    case Let(bindings, body) => deduce(body, env ++ bindings)
  }

def deduceUniop(op: Id, subject: Expression, env: Environment) =
  for
    maybeLambda <- lookup(op, env)
    opTy <- ensure[TypingError, LambdaType](maybeLambda, LookupError(op))
    subjectTy <- deduce(subject, env)
  yield opTy.apply(subjectTy)

def deduceBinop(op: Id, left: Expression, right: Expression, env: Environment) =
  for
    maybeLambda <- lookup(op, env)
    opTy <- ensure[TypingError, LambdaType](maybeLambda, LookupError(op))
    leftTy <- deduce(left, env)
    rightTy <- deduce(right, env)
  yield opTy.apply(leftTy, rightTy)

def deduceCond(cond: Cond, env: Environment) =
  def branchesAreOfEqualType(pass: Type, fail: Type) =
    if pass == fail
    then Right(None)
    else Left(CondMismatchError(cond, pass, fail))
  for
    pass <- deduce(cond.pass, env)
    fail <- deduce(cond.fail, env)
    _ <- branchesAreOfEqualType(pass, fail)
  yield pass

def lookup(id: Id, env: Environment): Either[TypingError, Type] =
  env.get(id.lexeme) match {
    case None => Left(LookupError(id))
    case Some(value) =>
      value.ty match {
        case None => deduce(value, env)
        case Some(ty) => Right(ty)
      }
  }
