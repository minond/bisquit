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
case class App(fn: Expression, args: List[Expression]) extends Expression
case class Let(bindings: Map[String, Expression], body: Expression) extends Expression
case class Cond(cond: Expression, pass: Expression, fail: Expression) extends Expression
case class Num(value: Double) extends Value
case class Bool(value: Boolean) extends Value

// XXX making values a List[Value] | List[Expression] will mean that now all
// Values will have to be potentially re-evaluated before they're used in any
// way since it's not clear if a list is still holding unevaluated expressions
// or not. Consider creating another list type that is an Expression and
// keeping Cons _just_ for Values.
case class Cons(values: List[Value] | List[Expression]) extends Value

case class Func(params: List[Id], body: Expression, scope: Scope = Map()) extends Value {
  def curried(bindings: List[Value]) =
    Func(params=params.drop(bindings.size),
         body=App(Func(params.take(bindings.size), body, scope), bindings),
         scope=scope)
}

case class Builtin(f: (List[Expression], Scope) => Either[RuntimeError, Value]) extends Value {
  def apply(args: List[Expression], scope: Scope): Either[RuntimeError, Value] =
    f(args, scope)
}
