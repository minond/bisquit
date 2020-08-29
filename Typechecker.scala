package bisquit
package typechecker

import scala.collection.mutable.{Map => MMap}
import scala.language.implicitConversions

import ast._
import scope._
import runtime._
import utils.ensure
import utils.Implicits.Eithers

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

case class TypeVariable(id: String) extends Type


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
case class UnificationError(ty1: Type, ty2: Type) extends TypingError


case class Substitution(substitutions: MMap[String, Type] = MMap()) {
  private val nums = LazyList.from(97).sliding(1)
  def fresh = TypeVariable(nums.next.head.toChar.toString)

  def apply(ty: Type): Type =
    ty match {
      case ty @ (UnitType | IntType | StrType | BoolType) => ty
      case LambdaType(tys) =>
        LambdaType(tys.map(apply))
      case TypeVariable(id) =>
        apply(substitutions.getOrElse(id, return ty))
    }

  def unify(ty1: Type, ty2: Type, force: Boolean = false): Either[TypingError, Substitution] =
    (ty1, ty2) match {
      case _ if ty1 == ty2 => Right(this)
      case (TypeVariable(id), _) => unifyMaybe(id, ty1, ty2, force)
      case (_, TypeVariable(id)) => unifyMaybe(id, ty2, ty1, force)
      case (LambdaType(tys1), LambdaType(tys2)) => unifyLambda(tys1, tys2)
      case (ty1, ty2) => Left(UnificationError(ty1, ty2))
    }

  private def unifyLambda(tys1: List[Type], tys2: List[Type]) =
    for
      _ <- tys1.zip(tys2).map { unify(_, _) }.squished()
    yield this

  private def unifyMaybe(id: String, ty1: Type, ty2: Type, force: Boolean) =
    if substitutions.contains(id) && !force
    then unify(ty2, ty1, true)
    else set(id, ty2)

  private def set(k: String, v: Type) =
    substitutions.addOne(k, v)
    Right(this)
}


def infer(expr: IR): Either[TypingError, Type] =
  infer(expr, Environment(), Substitution())

def infer(expr: IR, env: Environment, sub: Substitution): Either[TypingError, Type] =
  expr match {
    case _: Int => Right(IntType)
    case _: Str => Right(StrType)
    case _: Bool => Right(BoolType)
    case id : Id => lookup(id, env, sub)
    case Builtin(sig, _) => Right(sig)
    case cond : Cond => inferCond(cond, env, sub)
    case Let(bindings, body) => infer(pass1(body), env ++ bindings, sub)
    case Lambda(params, body, scope) => inferLambda(params, pass1(body), scope, env, sub)
    case App(fn, args) => inferApp(fn, args, env, sub)
  }

def inferApp(fn: Expression, args: List[Expression], env: Environment, sub: Substitution) =
  for
    tyArgs <- args.map(pass1).map(infer(_, env, sub)).squished()
    tyFn <- infer(pass1(fn), env, sub)
    tyRes = sub.fresh
    tySig = if tyArgs.isEmpty
            then List(UnitType)
            else tyArgs
    _ <- sub.unify(tyFn, LambdaType(tySig :+ tyRes))
  yield sub(tyRes)

def inferCond(cond: Cond, env: Environment, sub: Substitution) =
  def branchesAreOfEqualType(pass: Type, fail: Type) =
    if pass == fail
    then Right(None)
    else Left(CondMismatchError(cond, pass, fail))
  for
    pass <- infer(pass1(cond.pass), env, sub)
    fail <- infer(pass1(cond.fail), env, sub)
    _ <- branchesAreOfEqualType(pass, fail)
  yield pass

def inferLambda(params: List[Id], body: IR, scope: Environment, env: Environment, sub: Substitution) =
  val paramTys = params.map { _ => sub.fresh }

  val lexScope = params.zip(paramTys).foldLeft(env ++ scope) {
    case (acc, (id, ty)) =>
      acc ++ Map(id.lexeme -> Id(id.lexeme).typeTag(ty))
  }

  for
    tyBody <- infer(body, lexScope, sub)
    tyArgs = if paramTys.isEmpty
             then List(UnitType)
             else paramTys.map(sub(_))
  yield
    LambdaType(tyArgs :+ tyBody)


def lookup(id: Id, env: Environment, sub: Substitution): Either[TypingError, Type] =
  env.get(id.lexeme) match {
    case None => Left(LookupError(id))
    case Some(value) =>
      value.ty match {
        case None => infer(pass1(value), env, sub)
        case Some(ty) => Right(ty)
      }
  }
