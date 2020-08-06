package xyz.minond.bisquit.ast

import xyz.minond.bisquit.input.Positioned
import xyz.minond.bisquit.runtime.{Scope, RuntimeError}

sealed trait Token extends Positioned
sealed trait Expression extends Token
sealed trait Value extends Expression

case object Eof extends Token
case class Id(lexeme: String) extends Expression
case class Binop(op: Id, left: Expression, right: Expression) extends Expression
case class Uniop(op: Id, subject: Expression) extends Expression
case class App(fn: Id | Func, args: List[Expression]) extends Expression
case class Let(bindings: Map[String, Expression], body: Expression) extends Expression
case class Cond(cond: Expression, pass: Expression, fail: Expression) extends Expression
case class Num(value: Double) extends Value
case class Bool(value: Boolean) extends Value
case class Cons(values: List[Value]) extends Value

case class Func(params: List[Id], body: Expression) extends Value {
  def curried(bindings: List[Value]) =
    Func(params=params.drop(bindings.size),
         body=App(Func(params.take(bindings.size), body), bindings))
}

case class Builtin(f: (List[Expression], Scope) => Either[RuntimeError, Value]) extends Value {
  def apply(args: List[Expression], scope: Scope): Either[RuntimeError, Value] =
    f(args, scope)
}
