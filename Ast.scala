package bisquit
package ast

import scope._
import input.Positioned
import typechecker.{Typing, Typed, LambdaType}
import runtime.{eval, pass1, RuntimeError}

sealed trait Token extends Positioned
sealed trait Expression extends Positioned with Typing
sealed trait IR extends Typing
sealed trait Value extends Expression

case class Comma() extends Token
case class OpenParen() extends Token
case class CloseParen() extends Token
case class Colon() extends Token
case class Equal() extends Token

case class Id(lexeme: String) extends IR with Expression
case class Binop(op: Expression, left: Expression, right: Expression) extends Expression
case class Uniop(op: Expression, subject: Expression) extends Expression
case class App(fn: Expression, args: List[Expression]) extends IR with Expression
case class Let(bindings: Map[Id, Expression], body: Expression) extends IR with Expression
case class Cond(cond: Expression, pass: Expression, fail: Expression) extends IR with Expression
case class Int(value: Integer) extends IR with Value with Token
case class Str(value: String) extends IR with Value with Token
case class Bool(value: Boolean) extends IR with Value with Token
case class Record(fields: Map[Id, Expression]) extends IR with Value
case class RecordLookup(rec: Expression, field: Id) extends IR with Expression

case class Builtin(sig: LambdaType, fn: Callable.Func)
  extends IR
  with Value
  with Typed(sig)
  with Callable
  with Calling(fn)

case class Lambda(
  params: List[Id],
  body: Expression,
  boundScope: RuntimeScope = Map(),
) extends IR with Value with Callable {
  def apply(args: List[IR], scope: RuntimeScope): Either[RuntimeError, Value] =
    for
      vals <- eval(args, scope)
      ret <- evalIt(vals, scope)
    yield ret

  def evalIt(vals: List[Value], scope: RuntimeScope) =
    val argScope = params.map(_.lexeme).zip(vals).toMap
    val lexScope = scope ++ boundScope ++ argScope
    if params.size != vals.size
    then Right(curryIt(vals, lexScope))
    else eval(pass1(body), lexScope)

  def curryIt(bindings: List[Value], lexScope: RuntimeScope) =
    Lambda(params = params.drop(bindings.size),
           body = App(fn = Lambda(params = params.take(bindings.size),
                                  body = body,
                                  boundScope = boundScope),
                      args = bindings),
           boundScope = lexScope)
}


object Callable {
  type Func = (List[IR], RuntimeScope) =>
    Either[RuntimeError, Value]
}

trait Callable {
  def apply(args: List[IR], scope: RuntimeScope):
    Either[RuntimeError, Value]
}

trait Calling(fn: Callable.Func) {
  def apply(args: List[IR], scope: RuntimeScope) =
    fn(args, scope)
}
