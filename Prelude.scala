package bisquit
package prelude

import ast._
import printer._
import runtime._
import scope._
import typechecker._
import utils.ensure

val tyVar1 = fresh()
val polyNum = PolymorphicType(Some(NumType))
val polyOrd1 = PolymorphicType(Some(OrdType))
val polyOrd2 = PolymorphicType(Some(OrdType))

def signature(tys: List[Type], vars: List[PolymorphicType] = List.empty) =
  LambdaType(tys, vars)

// TODO Add ArgumentTypeError back:
// num <- ensure[RuntimeError, Int](value, ArgumentTypeError(expr))
def numericBinaryBuiltin(f: (Double, Double) => Double): Builtin =
  Builtin(signature(List(polyNum, polyNum, polyNum), List(polyNum)), {
    case (l :: r :: Nil, scope) =>
      for
        leftVal <- eval(l, scope)
        rightVal <- eval(r, scope)
      yield
        (leftVal, rightVal) match {
          case (Int(left), Int(right)) => Int(f(left.toDouble, right.toDouble).toInt)
          case (Real(left), Real(right)) => Real(f(left, right))
          case (Int(left), Real(right)) => Real(f(left.toDouble, right))
          case (Real(left), Int(right)) => Real(f(left, right.toDouble))
        }
  })

// TODO Add ArgumentTypeError back:
// num <- ensure[RuntimeError, Int](value, ArgumentTypeError(expr))
def numericUnaryBuiltin(f: Double => Double): Builtin =
  Builtin(signature(List(polyNum, polyNum)), {
    case (expr :: Nil, scope) =>
      for
        value <- eval(expr, scope)
      yield
        value match {
          case Int(num) => Int(f(num.toDouble).toInt)
          case Real(num) => Real(f(num))
        }
  })

val booleanAnd = Builtin(signature(List(BoolType, BoolType, BoolType)), {
  case (left :: right :: Nil, scope) =>
    eval(left, scope).flatMap {
      case Bool(true) => eval(right, scope)
      case Bool(false) => Right(Bool(false))
      case invalid => Left(ArgumentTypeError(pass1(invalid)))
    }
})

val booleanOr = Builtin(signature(List(BoolType, BoolType, BoolType)), {
  case (left :: right :: Nil, scope) =>
    eval(left, scope).flatMap {
      case Bool(true) => Right(Bool(true))
      case Bool(false) => eval(right, scope)
    }
})

val PreludeModuleName = Id("Prelude")
val PreludeFunctions = Map(
  Id("+") -> numericBinaryBuiltin(_ + _),
  Id("-") -> numericBinaryBuiltin(_ - _),
  Id("*") -> numericBinaryBuiltin(_ * _),
  Id("/") -> numericBinaryBuiltin(_ / _),
  Id("%") -> numericBinaryBuiltin(_ % _),
  Id("~") -> numericUnaryBuiltin(-_),
  Id("||") -> booleanOr,
  Id("&&") -> booleanAnd,

  Id("int_to_real") ->
    Builtin(signature(List(IntType, RealType)), {
      case (arg :: Nil, scope) =>
        eval(arg, scope).flatMap {
          case Int(int) => Right(Real(int.toDouble))
        }
    }),

  Id("eq") ->
    Builtin(signature(List(polyOrd1, polyOrd2, BoolType), List(polyOrd1, polyOrd2)), {
      case (l :: r :: Nil, scope) =>
        for
          left <- eval(l, scope)
          right <- eval(r, scope)
        yield
          Bool(left == right)
    }),

  Id("ref!") ->
    Builtin(signature(List(tyVar1, RefCellType(tyVar1))), {
      case (value :: Nil, scope) =>
        for
          evaled <- eval(value, scope)
        yield
          RefCell(evaled)
    }),

  Id("get!") ->
    Builtin(signature(List(RefCellType(tyVar1), tyVar1)), {
      case (ref :: Nil, scope) =>
        for
          evaled <- eval(ref, scope)
          refVal <- ensure[RuntimeError, RefCell](evaled, ArgumentTypeError(ref))
        yield
          refVal.value
    }),

  Id("set!") ->
    Builtin(signature(List(RefCellType(tyVar1), tyVar1, RefCellType(tyVar1))), {
      case (ref :: value :: Nil, scope) =>
        for
          maybeRefVal <- eval(ref, scope)
          refVal <- ensure[RuntimeError, RefCell](maybeRefVal, ArgumentTypeError(ref))
          evaled <- eval(value, scope)
        yield
          refVal.value = evaled
          RefCell(evaled)
    }),

  Id("universe") ->
    Builtin(signature(List(UnitType, UnitType)), { (_, scope) =>
      scope.map { case ((k, v)) =>
        println(s"${k.lexeme} : ${infer(pass1(v), scope, Substitution()).map(ty => formatted(ty)).right.get}")
      }
      Right(Tuple(List.empty))
    }),
)

val Prelude: Modules = Map(
  PreludeModuleName -> Module(PreludeModuleName,
                              PreludeFunctions.keys.toSet,
                              PreludeFunctions),
)
