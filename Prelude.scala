package bisquit
package prelude

import ast._
import runtime._
import scope._
import typechecker._
import utils.ensure

val polyNum = PolymorphicType(NumType)

def signature(tys: List[Type], vars: List[PolymorphicType] = List.empty) =
  LambdaType(tys, vars)

def numericBinaryBuiltin(f: (Integer, Integer) => Integer): Builtin =
  Builtin(signature(List(polyNum, polyNum, polyNum), List(polyNum)), {
    case (l :: r :: Nil, scope) =>
      for
        leftVal <- eval(l, scope)
        leftInt <- ensure[RuntimeError, Int](leftVal, ArgumentTypeError(l))
        rightVal <- eval(r, scope)
        rightInt <- ensure[RuntimeError, Int](rightVal, ArgumentTypeError(r))
      yield Int(f(leftInt.value, rightInt.value))
  })

def numericUnaryBuiltin(f: Integer => Integer): Builtin =
  Builtin(signature(List(polyNum, polyNum)), {
    case (expr :: Nil, scope) =>
      for
        value <- eval(expr, scope)
        num <- ensure[RuntimeError, Int](value, ArgumentTypeError(expr))
      yield Int(f(num.value))
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
)

val Prelude: Modules = Map(
  PreludeModuleName -> Module(PreludeModuleName,
                              PreludeFunctions.keys.toSet,
                              PreludeFunctions),
)
