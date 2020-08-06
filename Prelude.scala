package xyz.minond.bisquit.prelude

import xyz.minond.bisquit.ast.{Builtin, Num}

def numericBinaryBuiltin(f: (Double, Double) => Double): Builtin =
  Builtin({
    case Num(left) :: Num(right) :: Nil => Num(f(left, right))
  })

def numericUnaryBuiltin(f: Double => Double): Builtin =
  Builtin({
    case Num(right) :: Nil => Num(f(right))
  })

val Ops = Map(
  "+" -> numericBinaryBuiltin(_ + _),
  "-" -> numericBinaryBuiltin(_ - _),
  "*" -> numericBinaryBuiltin(_ * _),
  "/" -> numericBinaryBuiltin(_ / _),
  "%" -> numericBinaryBuiltin(_ % _),
  "~" -> numericUnaryBuiltin(-_),
)
