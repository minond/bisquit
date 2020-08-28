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

trait Typed(ty: Type) extends Typing {
  typeTag(ty)
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

case class PlaceholderType(id: Integer) extends Type

object PlaceholderType {
  private var currId = 0
  def fresh =
    currId += 1
    PlaceholderType(currId)
}


case class Substitution(substitutions: Map[scala.Int, Type]) {
  def apply(ty: Type): Type =
    ty match {
      case ty @ (UnitType | IntType | StrType | BoolType) => ty
      case LambdaType(tys) =>
        LambdaType(tys.map(apply))
      case PlaceholderType(id) =>
        substitutions.getOrElse(id, ty) match {
          case ty2 if ty2 == ty => ty
          case ty2: PlaceholderType => apply(ty2)
          case ty2 => ty2
        }
    }
}


sealed trait TypingError
case class LookupError(id: Id) extends TypingError
case class CondMismatchError(cond: Cond, pass: Type, fail: Type) extends TypingError


def infer(expr: Expression): Either[TypingError, Type] =
  infer(expr, Map())

def infer(expr: Expression, env: Environment): Either[TypingError, Type] =
  expr match {
    case _: Int => Right(IntType)
    case _: Str => Right(StrType)
    case _: Bool => Right(BoolType)
    case id : Id => lookup(id, env)
    case Builtin(sig, _) => Right(sig)
    case Uniop(op, subject) => inferUniop(op, subject, env)
    case Binop(op, left, right) => inferBinop(op, left, right, env)
    case cond : Cond => inferCond(cond, env)
    case Let(bindings, body) => infer(body, env ++ bindings)
    case Lambda(params, body, scope) =>
      Right(PlaceholderType.fresh)
  }

def inferUniop(op: Id, subject: Expression, env: Environment) =
  for
    maybeLambda <- lookup(op, env)
    opTy <- ensure[TypingError, LambdaType](maybeLambda, LookupError(op))
    subjectTy <- infer(subject, env)
  yield opTy.apply(subjectTy)

def inferBinop(op: Id, left: Expression, right: Expression, env: Environment) =
  for
    maybeLambda <- lookup(op, env)
    opTy <- ensure[TypingError, LambdaType](maybeLambda, LookupError(op))
    leftTy <- infer(left, env)
    rightTy <- infer(right, env)
  yield opTy.apply(leftTy, rightTy)

def inferCond(cond: Cond, env: Environment) =
  def branchesAreOfEqualType(pass: Type, fail: Type) =
    if pass == fail
    then Right(None)
    else Left(CondMismatchError(cond, pass, fail))
  for
    pass <- infer(cond.pass, env)
    fail <- infer(cond.fail, env)
    _ <- branchesAreOfEqualType(pass, fail)
  yield pass


def lookup(id: Id, env: Environment): Either[TypingError, Type] =
  env.get(id.lexeme) match {
    case None => Left(LookupError(id))
    case Some(value) =>
      value.ty match {
        case None => infer(value, env)
        case Some(ty) => Right(ty)
      }
  }

def signature(tys: Type*) =
  LambdaType(tys.toList)
