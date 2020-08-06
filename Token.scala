package xyz.minond.bisquit.token

import xyz.minond.bisquit.input.Positioned

sealed trait Token extends Positioned
sealed trait Expression extends Token
sealed trait Value

case object Eof
  extends Token

case class Id(lexeme: String)
  extends Expression

case class Binop(op: Id, left: Expression, right: Expression)
  extends Expression

case class Uniop(op: Id, subject: Expression)
  extends Expression

case class App(fn: Id | Func, args: List[Expression])
  extends Expression

case class Num(value: Double)
  extends Expression
  with Value

case class Func(params: List[Id], body: Expression, bindings: List[Value] = List())
  extends Expression
  with Value {
  def curry(newBindings: List[Value]) =
    Func(params, body, bindings ++ newBindings)
}
