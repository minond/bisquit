package bisquit
package typechecker

import ast._
import scope._
import runtime._
import utils.ensure

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

case class PlaceholderType(id: scala.Int) extends Type

object PlaceholderType {
  private var currId = 0
  def fresh =
    currId += 1
    PlaceholderType(currId)
}


trait Typing { self =>
  var ty: Option[Type] = None

  def typeTag(ty: Type): self.type =
    this.ty = Some(ty)
    this
}

trait Typed(ty: Type) extends Typing {
  typeTag(ty)
}


sealed trait TypingError
case class LookupError(id: Id) extends TypingError
case class CondMismatchError(cond: Cond, pass: Type, fail: Type) extends TypingError


case class Substitution(substitutions: Map[scala.Int, Type]) {
  def apply(ty: Type): Type =
    ty match {
      case ty @ (UnitType | IntType | StrType | BoolType) => ty
      case LambdaType(tys) =>
        LambdaType(tys.map(apply))
      case PlaceholderType(id) =>
        apply(substitutions.getOrElse(id, return ty))
    }

  def unify(ty1: Type, ty2: Type): Substitution =
    (ty1, ty2) match {
      case _ if ty1 == ty2 => this
      case (PlaceholderType(id), ty @ PlaceholderType) =>
        Substitution(substitutions ++ Map(id -> ty2))
      case (PlaceholderType(id), ty) =>
        Substitution(substitutions ++ Map(id -> ty))
      case (ty, PlaceholderType(id)) =>
        Substitution(substitutions ++ Map(id -> ty))
      case (LambdaType(tys1), LambdaType(tys2)) =>
        tys1.zip(tys2).foldLeft(this) {
          case (sub, (ty1, ty2)) => sub.unify(ty1, ty2)
        }
    }
}


def infer(expr: IR): Either[TypingError, Type] =
  infer(expr, Map())

def infer(expr: IR, env: Environment): Either[TypingError, Type] =
  expr match {
    case _: Int => Right(IntType)
    case _: Str => Right(StrType)
    case _: Bool => Right(BoolType)
    case id : Id => lookup(id, env)
    case Builtin(sig, _) => Right(sig)
    case cond : Cond => inferCond(cond, env)
    case Let(bindings, body) => infer(pass1(body), env ++ bindings)
    case Lambda(params, body, scope) => inferLambda(params, pass1(body), scope, env)
    case _: App => Right(PlaceholderType.fresh)
  }

def inferCond(cond: Cond, env: Environment) =
  def branchesAreOfEqualType(pass: Type, fail: Type) =
    if pass == fail
    then Right(None)
    else Left(CondMismatchError(cond, pass, fail))
  for
    pass <- infer(pass1(cond.pass), env)
    fail <- infer(pass1(cond.fail), env)
    _ <- branchesAreOfEqualType(pass, fail)
  yield pass

def inferLambda(params: List[Id], body: IR, scope: Environment, env: Environment) =
  val paramTys = params.map { _ => PlaceholderType.fresh }

  val lexScope = params.zip(paramTys).foldLeft(env ++ scope) {
    case (acc, (id, ty)) =>
      acc ++ Map(id.lexeme -> Id(id.lexeme).typeTag(ty))
  }

  for
    tyBody <- infer(body, lexScope)
  yield
    LambdaType(paramTys :+ tyBody)


def lookup(id: Id, env: Environment): Either[TypingError, Type] =
  env.get(id.lexeme) match {
    case None => Left(LookupError(id))
    case Some(value) =>
      value.ty match {
        case None => infer(pass1(value), env)
        case Some(ty) => Right(ty)
      }
  }
