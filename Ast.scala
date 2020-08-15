package xyz.minond.bisquit.ast

import xyz.minond.bisquit.scope._
import xyz.minond.bisquit.input.Positioned
import xyz.minond.bisquit.typechecker.{Typing, FuncType}
import xyz.minond.bisquit.runtime.RuntimeError

sealed trait Token extends Positioned
sealed trait Expression extends Token with Typing
sealed trait Value extends Expression

case class Id(lexeme: String) extends Expression
case class Binop(op: Id, left: Expression, right: Expression) extends Expression
case class Uniop(op: Id, subject: Expression) extends Expression
case class App(fn: Expression, args: List[Expression]) extends Expression
case class Let(bindings: Map[String, Expression], body: Expression) extends Expression
case class Cond(cond: Expression, pass: Expression, fail: Expression) extends Expression
case class Num(value: Double) extends Value
case class Bool(value: Boolean) extends Value
case class Cons(values: List[Expression]) extends Value

case class Func(params: List[Id], body: Expression, scope: RuntimeScope = Map()) extends Value {
  def curried(bindings: List[Value]) =
    Func(params=params.drop(bindings.size),
         body=App(Func(params.take(bindings.size), body, scope), bindings),
         scope=scope)
}

type Callable = (List[Expression], RuntimeScope) => Either[RuntimeError, Value]
case class Builtin(sig: FuncType, f: Callable) extends Value {
  typeTag(sig)

  def apply(args: List[Expression], scope: RuntimeScope): Either[RuntimeError, Value] =
    f(args, scope)
}
