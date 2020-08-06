package xyz.minond.bisquit.prelude

import xyz.minond.bisquit.ast._
import xyz.minond.bisquit.runtime._

def numericBinaryBuiltin(f: (Double, Double) => Double): Builtin =
  Builtin({
    case Num(left) :: Num(right) :: Nil => Right(Num(f(left, right)))
  })

def numericUnaryBuiltin(f: Double => Double): Builtin =
  Builtin({
    case Num(right) :: Nil => Right(Num(f(right)))
  })

val booleanAnd = LazyBuiltin({
  case (left :: right :: Nil, scope) =>
    eval(left, scope).flatMap {
      case Bool(true) => eval(right, scope)
      case Bool(false) => Right(Bool(false))
    }
})

val booleanOr = LazyBuiltin({
  case (left :: right :: Nil, scope) =>
    eval(left, scope).flatMap {
      case Bool(true) => Right(Bool(true))
      case Bool(false) => eval(right, scope)
    }
})

val Ops = Map(
  "+" -> numericBinaryBuiltin(_ + _),
  "-" -> numericBinaryBuiltin(_ - _),
  "*" -> numericBinaryBuiltin(_ * _),
  "/" -> numericBinaryBuiltin(_ / _),
  "%" -> numericBinaryBuiltin(_ % _),
  "~" -> numericUnaryBuiltin(-_),
  "||" -> booleanOr,
  "&&" -> booleanAnd,
)
