package bisquit
package ast

import scope._
import input.Positioned
import typechecker.{Typing, Typed, LambdaType}
import runtime.RuntimeError

sealed trait Token extends Positioned
sealed trait Expression extends Token with Typing
sealed trait Value extends Expression

case class Id(lexeme: String) extends Expression
case class Binop(op: Id, left: Expression, right: Expression) extends Expression
case class Uniop(op: Id, subject: Expression) extends Expression
case class App(fn: Expression, args: List[Expression]) extends Expression
case class Let(bindings: Map[String, Expression], body: Expression) extends Expression
case class Cond(cond: Expression, pass: Expression, fail: Expression) extends Expression
case class Int(value: Integer) extends Value
case class Str(value: String) extends Value
case class Bool(value: Boolean) extends Value

object Callable {
  type Func = (List[Expression], RuntimeScope) =>
    Either[RuntimeError, Value]
}

trait Callable {
  val apply: Callable.Func
}

case class Lambda(params: List[Id], body: Expression, scope: RuntimeScope = Map()) extends Value {
  def curried(bindings: List[Value]) =
    Lambda(params=params.drop(bindings.size),
         body=App(Lambda(params.take(bindings.size), body, scope), bindings),
         scope=scope)
}

case class Builtin(sig: LambdaType, apply: Callable.Func)
  extends Value
  with Typed(sig)
  with Callable
