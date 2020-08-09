package xyz.minond.bisquit.prelude

import xyz.minond.bisquit.ast._
import xyz.minond.bisquit.runtime._
import xyz.minond.bisquit.typer.{BoolTy, NumTy, Signature}
import xyz.minond.bisquit.utils.ensure

def numericBinaryBuiltin(f: (Double, Double) => Double): Builtin =
  Builtin(Signature(NumTy, NumTy, NumTy), {
    case (l :: r :: Nil, scope) =>
      for
        leftVal <- eval(l, scope)
        leftNum <- ensure[RuntimeError, Num](leftVal, ArgumentTypeError(l))
        rightVal <- eval(r, scope)
        rightNum <- ensure[RuntimeError, Num](rightVal, ArgumentTypeError(r))
      yield Num(f(leftNum.value, rightNum.value))
  })

def numericUnaryBuiltin(f: Double => Double): Builtin =
  Builtin(Signature(NumTy, NumTy), {
    case (expr :: Nil, scope) =>
      for
        value <- eval(expr, scope)
        num <- ensure[RuntimeError, Num](value, ArgumentTypeError(expr))
      yield Num(f(num.value))
  })

val booleanAnd = Builtin(Signature(BoolTy, BoolTy, BoolTy), {
  case (left :: right :: Nil, scope) =>
    eval(left, scope).flatMap {
      case Bool(true) => eval(right, scope)
      case Bool(false) => Right(Bool(false))
    }
})

val booleanOr = Builtin(Signature(BoolTy, BoolTy, BoolTy), {
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
