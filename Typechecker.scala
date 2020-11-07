package bisquit
package typechecker

import scala.collection.mutable.{Map => MMap}
import scala.language.implicitConversions

import errors._
import nodes.{Int => _, _}
import runtime._
import scope._
import utils.Implicits.Eithers
import utils.{ensure, formap, remap}


sealed trait Type(val containedSets: Type*) {
  val containedSetsSize = containedSets.size
  def contains(ty: Type): Boolean =
    ty == this || containedSets.takeWhile(!_.contains(ty)).size != containedSetsSize
  def contained(ty: Type): Boolean =
    false

  var tok: Option[IR | Expression | Token] = None
  def setToken(token: IR | Expression | Token): Type =
    this.tok = Some(token)
    this
}

case class PolymorphicType(
    concreteType: Option[Type],
    val tyVar: TypeVariable = fresh(),
) extends Type()

case class UnitType() extends Type()
case class StrType() extends Type()
case class BoolType() extends Type()
case class RecordType(fields: Map[Id, Type] = Map()) extends Type()

case class IntType() extends Type()
case class RealType() extends Type(IntType())
case class NumType() extends Type(RealType())

case class ListaType(of: Type) extends Type() {
  override def contained(ty: Type): Boolean =
    ty.contains(of)
}

case class TupleType(fields: List[Type]) extends Type() {
  override def contained(ty: Type): Boolean =
    fields.forall(field => ty.contains(field))
}

case class OrdType() extends Type(NumType(), StrType(), BoolType())
case class RefCellType(of: Type) extends Type()

case class LambdaType(tys: List[Type], vars: List[PolymorphicType] = List.empty) extends Type() {
  def apply(args: Type*): Type =
    apply(args.toList)

  def apply(args: List[Type]): Type =
    LambdaType(tys.drop(args.size), vars).flatten

  def flatten =
    if tys.size == 1
    then tys.head
    else this
}

case class TypeVariable(id: Int) extends Type()
case class RecordVariable(fields: MMap[Id, Type] = MMap()) extends Type()


trait Typing { self =>
  var ty: Option[Type] = None

  def typeTag(ty: Type): self.type =
    this.ty = Some(ty)
    this
}

trait Typed(ty: Type) extends Typing {
  typeTag(ty)
}


val freshIds = LazyList.from(1).sliding(1)

def fresh() =
  TypeVariable(freshIds.next.head)

def freshRecord(fields: (Id, Type)*) =
  val rec = RecordVariable()
  for field <- fields do rec.fields.addOne(field)
  rec

case class Substitution(substitutions: MMap[Int, Type] = MMap()) {
  def apply(ty: Type): Type =
    ty match {
      case ty : (UnitType | NumType | IntType | RealType | StrType | BoolType) => ty
      case RefCellType(ty) =>
        RefCellType(apply(ty))
      case ty : PolymorphicType =>
        substitutions.getOrElse(ty.tyVar.id, ty)
      case ListaType(of) =>
        ListaType(apply(of))
      case TupleType(fields) =>
        TupleType(fields.map(apply))
      case rec: RecordVariable =>
        RecordVariable(MMap((remap(rec.fields.toMap) { apply }).toList:_*))
      case RecordType(fields) =>
        RecordType(remap(fields) { apply })
      case LambdaType(tys, vars) =>
        LambdaType(tys.map(apply), vars)
      case TypeVariable(id) =>
        substitutions.get(id) match {
          case None => ty
          case Some(sub1) =>
            sub1 match {
              case next @ TypeVariable(id2) =>
                substitutions.get(id2) match {
                  case Some(TypeVariable(id3)) if id3 == id => ty
                  case Some(ty2: TypeVariable) => apply(ty2)
                  case Some(ListaType(ty2)) if ty2 == ty => ty
                  case Some(ty2) => apply(ty2)
                  case None => apply(next)
                }

              case nextTy => apply(nextTy)
            }
        }
    }

  def unify(ty1: Type, ty2: Type): Either[TypingError, Substitution] =
    (ty1, ty2) match {
      case _ if ty1 == ty2 => Right(this)
      case _ if ty1.contains(ty2) => Right(this)
      case _ if ty2.contained(ty1) => Right(this)

      case (TypeVariable(id), TypeVariable(_)) => unifyVar(id, ty1, ty2)
      case (TypeVariable(id), ty) =>
        substitutions.get(id) match {
          case None => set(id, ty)
          case Some(tyVar : TypeVariable) =>
            set(id, ty)
            unify(tyVar, ty)
          case Some(ty2) =>
            unify(ty, ty2)
        }

      case (ty, tyVar : TypeVariable) =>
        unify(tyVar, ty)


      case (ListaType(tyVar : TypeVariable), ListaType(ty2)) =>
        unify(tyVar, ty2)

      case (ListaType(ty1), ListaType(tyVar : TypeVariable)) =>
        unify(ty1, tyVar)

      case (ty1 @ PolymorphicType(None, _), _) =>
        substitutions.get(ty1.tyVar.id) match {
          case None =>
            // This polymorphic type has not been unified before, so we simply
            // unify its type variable to the second type.
            unify(ty1.tyVar, ty2)

          case Some(subbedTy1) =>
            // This polymorphic type has been unified before, so we ask whether
            // or not the type that it _was_ unified to can be unified to the
            // type we're being unified to now.
            unify(subbedTy1, ty2)
        }

      case (ty1 @ PolymorphicType(Some(concreteType1), _), _) =>
        substitutions.get(ty1.tyVar.id) match {
          case None =>
            // This polymorphic type has not been unified before, so we ask
            // whether its concrete type can be unified to the type we're being
            // unified to now.
            unify(concreteType1, ty2).flatMap { s =>
              substitutions.get(ty1.tyVar.id) match {
                case None =>
                  // At this point the polymorphic type's concrete type can be
                  // unified to the second type, so we unify the type variable
                  // that represents the polymorphic type to the second type.
                  // With this, the polymorphic "becomes" the second type.
                  s.unify(ty1.tyVar, apply(ty2))

                case Some(subbedTy1) =>
                  // Concrete type unified, polymorphic type unified, we're
                  // done.
                  Right(s)
              }
            }

          case Some(subbedTy1) =>
            // This polymorphic type has been unified before, so we ask whether
            // or not the type that it _was_ unified to can be unified to the
            // type we're being unified to now.
            unify(subbedTy1, ty2)
        }

      case (ty1, polyTy : PolymorphicType) =>
        unify(polyTy, ty1)

      case (RefCellType(ty1), RefCellType(ty2)) =>
        unify(ty1, ty2)

      case (LambdaType(tys1, _), LambdaType(tys2, _)) => unifyLambda(tys1, tys2)
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
           then {
             substitutions.get(id.get) match {
               case None =>
                 set(id.get, s1)
               case Some(tyVar : TypeVariable) =>
                 unify(tyVar, s1)
               case Some(ty) =>
                 unify(s1, ty)
             }
           } else Right(this)
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
    substitutions.get(id) match {
      case None => set(id, ty)
      case Some(ty2 @ TypeVariable(id2)) =>
        substitutions.get(id2) match {
          case None => unify(ty2, ty, id)

          case Some(ty3 @ TypeVariable(id3)) =>
            if id3 == id
            then Right(this)
            else unify(apply(tyVar), ty, id)

          case Some(ty) =>
            unify(apply(tyVar), ty, id)
        }

      case Some(next) => unify(next, ty, id)
    }

  private def set(k: Int, v: Type) =
    substitutions.addOne(k, v)
    Right(this)
}


def infer(expr: IR): Either[TypingError, Type] =
  infer(expr, Map(), Substitution())

def infer(stmt: Statement, env: Environment, sub: Substitution): Either[TypingError, Type] =
  stmt match {
    case Definition(id, value) =>
      val rec = env ++ Map(id -> Id(id.lexeme).typeTag(fresh()))
      infer(pass1(value), rec, sub)

    case _: (Import | Module) =>
      Right(BoolType())
  }

def infer(expr: IR, env: Environment, sub: Substitution): Either[TypingError, Type] =
  expr match {
    case _: nodes.Int => Right(IntType().setToken(expr))
    case _: Real => Right(RealType().setToken(expr))
    case _: Str => Right(StrType().setToken(expr))
    case _: Bool => Right(BoolType().setToken(expr))
    case id : Id => lookup(id, env, sub).map(_.setToken(expr))
    case Builtin(sig, _) => Right(sig)
    case cond : Cond => inferCond(cond, env, sub)
    case Let(bindings, body) => inferLet(bindings, body, env, sub)
    case Lambda(params, body, scope) => inferLambda(params, pass1(body), scope.getOrElse(Map()), env, sub)
    case App(fn, args) => inferApp(fn, args, env, sub)
    case Lista(items) => inferLista(items, env, sub)
    case Tuple(fields) => inferTuple(fields, env, sub)
    case RefCell(value) => inferRefCell(value, env, sub)
    case Record(fields) => inferRecord(fields, env, sub)
    case RecordLookup(rec, field) => inferRecordLookup(rec, field, env, sub)
  }

def inferAll(exprs: List[Expression], env: Environment, sub: Substitution) =
  for
    tyExprs <- exprs.map(pass1).map(infer(_, env, sub)).squished()
  yield
    tyExprs

def inferRefCell(value: Value, env: Environment, sub: Substitution) =
  for
    ty <- infer(value.asInstanceOf[IR], env, sub)
  yield
    RefCellType(ty)

def inferLista(items: List[Expression], env: Environment, sub: Substitution) =
  for
    tys <- inferAll(items, env, sub)
    /* TODO Ensure all types are equal */
  yield
    if tys.size == 0
    then ListaType(fresh())
    else ListaType(tys.head)

def inferTuple(fields: List[Expression], env: Environment, sub: Substitution) =
  for
    tys <- inferAll(fields, env, sub)
  yield
    if tys.size == 0
    then UnitType()
    else TupleType(tys)

def inferLet(bindings: Map[Id, Expression], body: Expression, env: Environment, sub: Substitution) =
  val bindingTys = bindings.keys.toList.map { id =>
    id.ty match {
      case None => fresh()
      case Some(ty) => ty
    }
  }

  val unboundLexScope = bindings.keys.toList.zip(bindingTys).foldLeft(env) {
    case (acc, (id, ty)) =>
      acc ++ Map(id -> Id(id.lexeme).typeTag(ty))
  }

  val boundLexScope = bindings.foldLeft[Either[TypingError, Environment]](Right(unboundLexScope)) {
    case (acc, (id, expr)) =>
      acc.flatMap { innerscope =>
        infer(pass1(expr), innerscope, sub).map { v =>
          id.ty match {
            case None =>
              innerscope ++ Map(id -> expr.typeTag(v))
            case Some(ty) =>
              innerscope ++ Map(id -> expr.typeTag(ty))
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
    fieldTy = fresh()
    recTy = freshRecord(field -> fieldTy)
    _ <- sub.unify(record, recTy)
    ty <- lookup(field, recTy.fields.toMap, RecordLookupError(field, record))
  yield sub(ty)

def inferRecord(fields: Map[Id, Expression], env: Environment, sub: Substitution) =
  for
    inners <- formap(fields){ (k, v) =>
      infer(pass1(v), env, sub)
    }
    ret = RecordVariable(MMap(inners.toList:_*))
  yield ret

def inferApp(fn: Expression, args: List[Expression], env: Environment, sub: Substitution) =
  args.map(pass1).map(infer(_, env, sub)).squished().flatMap { inferredTys =>
    val tyArgs = if inferredTys.isEmpty
                 then List(UnitType())
                 else inferredTys

    infer(pass1(fn), env, sub).flatMap { maybeFn =>
      maybeFn match {
        case tyFn @ LambdaType(tyFnSig, tyVars) =>
          if tyArgs.size > tyFnSig.size - 1
          then Left(TooManyArguments(tyFn, args))
          else if tyArgs.size == tyFn.tys.size - 1
          then
            val tyRes = fresh()
            val tySig = if tyArgs.isEmpty
                        then List(UnitType(), tyRes)
                        else tyArgs :+ tyRes

            sub.unify(tyFn, LambdaType(tySig)).map { _ =>
              sub(tyRes)
            }
          else
            val partialArgs = tyFnSig.take(args.size)
            val leftoverArgs = tyFnSig.drop(args.size)

            sub.unify(LambdaType(partialArgs), LambdaType(tyArgs)).map { _ =>
              sub(LambdaType(leftoverArgs, tyVars))
            }

        case _ =>
          for
            tyArgs <- args.map(pass1).map(infer(_, env, sub)).squished()
            tyFn <- infer(pass1(fn), env, sub)
            tyRes = fresh()
            tySig = if tyArgs.isEmpty
                    then List(UnitType())
                    else tyArgs
            _ <- sub.unify(tyFn, LambdaType(tySig :+ tyRes))
          yield sub(tyRes)
      }
    }
  }


def inferCond(cond: Cond, env: Environment, sub: Substitution) =
  for
    condTy <- infer(pass1(cond.cond), env, sub)
    _ <- sub.unify(condTy, BoolType())
    passTy <- infer(pass1(cond.pass), env, sub)
    failTy <- infer(pass1(cond.fail), env, sub)
    _ <- sub.unify(passTy, failTy)
  yield sub(passTy)

def inferLambda(params: List[Id], body: IR, scope: Scope, env: Environment, sub: Substitution) =
  val paramTys = params.map { param =>
    param.ty match {
      case None => fresh()
      case Some(ty) => ty
    }
  }

  val lexScope = params.zip(paramTys).foldLeft(env ++ scope) {
    case (acc, (id, ty)) =>
      acc ++ Map(id -> Id(id.lexeme).typeTag(ty))
  }

  for
    tyBody <- infer(body, lexScope, sub)
    tyArgs = if paramTys.isEmpty
             then List(UnitType())
             else paramTys.map(sub(_))
    tyVars = sub.substitutions.foldLeft[List[PolymorphicType]](List.empty) {
               case (acc, (_, ty : PolymorphicType)) => acc :+ ty
               case (acc, _) => acc
             }
  yield
    LambdaType(tyArgs :+ tyBody, tyVars.toSet.toList)


def lookup[V, L](id: Id, scope: Map[Id, V], left: => L): Either[L, V] =
  scope.get(id) match {
    case None => Left(left)
    case Some(value) => Right(value)
  }

def lookup(id: Id, env: Environment, sub: Substitution): Either[TypingError, Type] =
  env.get(id) match {
    case None => Left(LookupError(id))
    case Some(value) =>
      value.ty match {
        case None => infer(pass1(value), env ++ Map(id -> Id(id.lexeme).typeTag(fresh())), sub)
        case Some(ty) => Right(ty)
      }
  }
