package bisquit
package typechecker

import scala.collection.mutable.{Map => MMap}
import scala.language.implicitConversions

import ast.{Int => _, _}
import scope._
import runtime._
import utils.{ensure, formap, remap}
import utils.Implicits.Eithers

sealed trait Type
case object UnitType extends Type
case object IntType extends Type
case object StrType extends Type
case object BoolType extends Type
case class RecordType(fields: Map[Id, Type] = Map()) extends Type

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

case class TypeVariable(id: Int) extends Type
case class RecordVariable(fields: MMap[Id, Type] = MMap()) extends Type


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
case class UnificationError(ty1: Type, ty2: Type) extends TypingError
case class ExpectedRecordInstead(got: Type) extends TypingError
case class RecordLookupError(id: Id, record: Type) extends TypingError


case class Substitution(substitutions: MMap[Int, Type] = MMap()) {
  private val nums = LazyList.from(1).sliding(1)
  def fresh = TypeVariable(nums.next.head)
  def freshRecord(fields: (Id, Type)*) =
    val rec = RecordVariable()
    for field <- fields do rec.fields.addOne(field)
    rec

  def apply(ty: Type): Type =
    ty match {
      case ty @ (UnitType | IntType | StrType | BoolType | RecordType) => ty
      case rec: RecordVariable =>
        RecordVariable(MMap((remap(rec.fields.toMap) { apply }).toList:_*))
      case RecordType(fields) =>
        RecordType(remap(fields) { apply })
      case LambdaType(tys) =>
        LambdaType(tys.map(apply))
      case TypeVariable(id) =>
        apply(substitutions.getOrElse(id, return ty))
    }

  def unify(ty1: Type, ty2: Type): Either[TypingError, Substitution] =
    (ty1, ty2) match {
      case _ if ty1 == ty2 => Right(this)
      case (TypeVariable(id), _) => unifyVar(id, ty1, ty2)
      case (_, TypeVariable(id)) => unifyVar(id, ty2, ty1)
      case (LambdaType(tys1), LambdaType(tys2)) => unifyLambda(tys1, tys2)
      case (record: RecordType, recVar: RecordVariable) => unifyRecordToRecVar(record, recVar)
      case (s1: RecordVariable, s2: RecordVariable) => unifyRecVarToRecVar(s1, s2, None)
      case (ty1, ty2) => Left(UnificationError(ty1, ty2))
    }

  private def unify(ty1: Type, ty2: Type, id: Int): Either[TypingError, Substitution] =
    (ty1, ty2) match {
      case (s1: RecordVariable, s2: RecordVariable) => unifyRecVarToRecVar(s1, s2, Some(id))
      case (ty1, ty2) => unify(ty1, ty2)
    }

  private def unifyRecVarToRecVar(s1: RecordVariable, s2: RecordVariable, id: Option[Int]) =
    for
      _ <- s1.fields.toList.map {
             case (field, ty1) =>
               s2.fields.get(field) match {
                 case None =>
                   s2.fields.addOne(field, ty1)
                   Right(this)
                 case Some(ty2) => unify(ty1, ty2)
               }
           }.squished()
      _ <- s2.fields.toList.map {
             case (field, ty1) =>
               s1.fields.get(field) match {
                 case None =>
                   s1.fields.addOne(field, ty1)
                   Right(this)
                 case Some(ty2) => unify(ty1, ty2)
               }
           }.squished()
      _ <- if id.isDefined
           then set(id.get, s1)
           else Right(this)
    yield this

  private def unifyRecordToRecVar(record: RecordType, recVar: RecordVariable) =
    for
      _ <- recVar.fields.toList.map {
             case (field, ty1) =>
               record.fields.get(field) match {
                 case None => Left(UnificationError(record, recVar)) // XXX return missing key error
                 case Some(ty2) => unify(ty1, ty2)
               }
           }.squished()
    yield this

  private def unifyLambda(tys1: List[Type], tys2: List[Type]) =
    for
      _ <- tys1.zip(tys2).map { unify(_, _) }.squished()
    yield this

  private def unifyVar(id: Int, tyVar: Type, ty: Type) =
    if substitutions.contains(id)
    then unify(apply(tyVar), ty, id)
    else set(id, ty)

  private def set(k: Int, v: Type) =
    substitutions.addOne(k, v)
    Right(this)
}


def infer(expr: IR): Either[TypingError, Type] =
  infer(expr, Environment(), Substitution())

def infer(expr: IR, env: Environment, sub: Substitution): Either[TypingError, Type] =
  expr match {
    case _: ast.Int => Right(IntType)
    case _: Str => Right(StrType)
    case _: Bool => Right(BoolType)
    case id : Id => lookup(id, env, sub)
    case Builtin(sig, _) => Right(sig)
    case cond : Cond => inferCond(cond, env, sub)
    case Let(bindings, body) => inferLet(bindings, body, env, sub)
    case Lambda(params, body, scope) => inferLambda(params, pass1(body), scope, env, sub)
    case App(fn, args) => inferApp(fn, args, env, sub)
    case Record(fields) => inferRecord(fields, env, sub)
    case RecordLookup(rec, field) => inferRecordLookup(rec, field, env, sub)
  }

def inferLet(bindings: Map[Id, Expression], body: Expression, env: Environment, sub: Substitution) =
  val bindingTys = bindings.keys.toList.map { id =>
    id.ty match {
      case None => sub.fresh
      case Some(ty) => ty
    }
  }

  val unboundLexScope = bindings.keys.toList.zip(bindingTys).foldLeft(env) {
    case (acc, (id, ty)) =>
      acc ++ Map(id.lexeme -> Id(id.lexeme).typeTag(ty))
  }

  val boundLexScope = bindings.foldLeft[Either[TypingError, Environment]](Right(unboundLexScope)) {
    case (acc, (id, expr)) =>
      acc.flatMap { innerscope =>
        infer(pass1(expr), innerscope, sub).map { v =>
          id.ty match {
            case None =>
              innerscope ++ Map(id.lexeme -> expr.typeTag(v))
            case Some(ty) =>
              innerscope ++ Map(id.lexeme -> expr.typeTag(ty))
          }
        }
      }
  }

  for
    scope <- boundLexScope
    ret <- infer(pass1(body), scope, sub)
  yield sub(ret)

def inferRecordLookup(rec: Expression, field: Id, env: Environment, sub: Substitution) =
  for
    record <- infer(pass1(rec), env, sub)
    fieldTy = sub.fresh
    recTy = sub.freshRecord(field -> fieldTy)
    _ <- sub.unify(record, recTy)
    ty <- lookup(field, recTy.fields.toMap, RecordLookupError(field, record))
  yield ty

def inferRecord(fields: Map[Id, Expression], env: Environment, sub: Substitution) =
  for
    inners <- formap(fields){ v => infer(pass1(v), env, sub) }
    ret = RecordVariable(MMap(inners.toList:_*))
  yield ret

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
  for
    condTy <- infer(pass1(cond.cond), env, sub)
    _ <- sub.unify(condTy, BoolType)
    passTy <- infer(pass1(cond.pass), env, sub)
    failTy <- infer(pass1(cond.fail), env, sub)
    _ <- sub.unify(passTy, failTy)
  yield sub(passTy)

def inferLambda(params: List[Id], body: IR, scope: Environment, env: Environment, sub: Substitution) =
  val paramTys = params.map { param =>
    param.ty match {
      case None => sub.fresh
      case Some(ty) => ty
    }
  }

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


def lookup[V, L](id: Id, scope: Map[Id, V], left: => L): Either[L, V] =
  scope.get(id) match {
    case None => Left(left)
    case Some(value) => Right(value)
  }

def lookup(id: Id, env: Environment, sub: Substitution): Either[TypingError, Type] =
  env.get(id.lexeme) match {
    case None => Left(LookupError(id))
    case Some(value) =>
      value.ty match {
        case None => infer(pass1(value), env, sub)
        case Some(ty) => Right(ty)
      }
  }
