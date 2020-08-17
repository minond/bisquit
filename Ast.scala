package bisquit
package ast

import scope._
import input.Positioned
import typechecker.{Typing, Typed, LambdaType}
import runtime.{eval, RuntimeError}

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
  def apply(args: List[Expression], scope: RuntimeScope):
    Either[RuntimeError, Value]
}

trait Calling(fn: Callable.Func) {
  def apply(args: List[Expression], scope: RuntimeScope) =
    fn(args, scope)
}

case class Lambda(
  params: List[Id],
  body: Expression,
  boundScope: RuntimeScope = Map(),
) extends Value with Callable {
  def apply(args: List[Expression], scope: RuntimeScope): Either[RuntimeError, Value] =
    for
      vals <- eval(args, scope)
      ret <- evalIt(vals)
    yield ret

  def evalIt(vals: List[Value]) =
    val argScope = params.map(_.lexeme).zip(vals).toMap
    val lexScope = boundScope ++ argScope
    if params.size != vals.size
    then Right(curryIt(vals, lexScope))
    else eval(body, lexScope)

  def curryIt(bindings: List[Value], lexScope: RuntimeScope) =
    Lambda(params = params.drop(bindings.size),
           body = App(fn = Lambda(params = params.take(bindings.size),
                                  body = body,
                                  boundScope = boundScope),
                      args = bindings),
           boundScope = lexScope)
}

case class Builtin(sig: LambdaType, fn: Callable.Func)
  extends Value
  with Typed(sig)
  with Calling(fn)
