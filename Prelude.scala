package bisquit
package prelude

import ast._
import runtime._
import scope._
import typechecker._
import utils.ensure

def signature(tys: Type*) =
  LambdaType(tys.toList)

def numericBinaryBuiltin(f: (Integer, Integer) => Integer): Builtin =
  Builtin(signature(IntType, IntType, IntType), {
    case (l :: r :: Nil, scope) =>
      for
        leftVal <- eval(l, scope)
        leftInt <- ensure[RuntimeError, Int](leftVal, ArgumentTypeError(l))
        rightVal <- eval(r, scope)
        rightInt <- ensure[RuntimeError, Int](rightVal, ArgumentTypeError(r))
      yield Int(f(leftInt.value, rightInt.value))
  })

def numericUnaryBuiltin(f: Integer => Integer): Builtin =
  Builtin(signature(IntType, IntType), {
    case (expr :: Nil, scope) =>
      for
        value <- eval(expr, scope)
        num <- ensure[RuntimeError, Int](value, ArgumentTypeError(expr))
      yield Int(f(num.value))
  })

val booleanAnd = Builtin(signature(BoolType, BoolType, BoolType), {
  case (left :: right :: Nil, scope) =>
    eval(left, scope).flatMap {
      case Bool(true) => eval(right, scope)
      case Bool(false) => Right(Bool(false))
      case invalid => Left(ArgumentTypeError(pass1(invalid)))
    }
})

val booleanOr = Builtin(signature(BoolType, BoolType, BoolType), {
  case (left :: right :: Nil, scope) =>
    eval(left, scope).flatMap {
      case Bool(true) => Right(Bool(true))
      case Bool(false) => eval(right, scope)
    }
})

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
  Id("Prelude") -> Module(Id("Prelude"), PreludeFunctions.keys.toSet, PreludeFunctions),
)
