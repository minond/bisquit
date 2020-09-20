package bisquit
package ast

import scope._
import input._
import typechecker._
import runtime._

sealed trait Token extends Positioned
sealed trait Expression extends Positioned with Typing
sealed trait IR extends Typing
sealed trait Value extends Expression
sealed trait Statement { def asExpression(scope: Scope, modules: Modules): Expression }

case class Eof() extends Token
case class Comma() extends Token
case class OpenParen() extends Token
case class CloseParen() extends Token
case class OpenCurlyBraket() extends Token
case class CloseCurlyBraket() extends Token
case class Colon() extends Token
case class Equal() extends Token
case class Dot() extends Token

case class Id(lexeme: String) extends IR with Expression with Token
case class Binop(op: Expression, left: Expression, right: Expression) extends Expression
case class Uniop(op: Expression, subject: Expression) extends Expression
case class App(fn: Expression, args: List[Expression]) extends IR with Expression
case class Let(bindings: Map[Id, Expression], body: Expression) extends IR with Expression
case class Cond(cond: Expression, pass: Expression, fail: Expression) extends IR with Expression
case class Int(value: Integer) extends IR with Value with Token
case class Str(value: String) extends IR with Value with Token
case class Bool(value: Boolean) extends IR with Value with Token
case class Tuple(fields: List[Expression]) extends IR with Value
case class Record(fields: Map[Id, Expression] = Map()) extends IR with Value
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
  boundScope: Scope = Map(),
) extends IR with Value with Callable {
  def apply(args: List[IR], scope: Scope): Either[RuntimeError, Value] =
    for
      vals <- eval(args, scope)
      ret <- evalIt(vals, scope)
    yield ret

  def evalIt(vals: List[Value], scope: Scope) =
    val argScope = params.zip(vals).toMap
    val lexScope = scope ++ boundScope ++ argScope
    if params.size != vals.size
    then Right(curryIt(vals, lexScope))
    else eval(pass1(body), lexScope)

  def curryIt(bindings: List[Value], lexScope: Scope) =
    Lambda(params = params.drop(bindings.size),
           body = App(fn = Lambda(params = params.take(bindings.size),
                                  body = body,
                                  boundScope = boundScope),
                      args = bindings),
           boundScope = lexScope)
}


object Callable {
  type Func = (List[IR], Scope) =>
    Either[RuntimeError, Value]
}

trait Callable {
  def apply(args: List[IR], scope: Scope):
    Either[RuntimeError, Value]
}

trait Calling(fn: Callable.Func) {
  def apply(args: List[IR], scope: Scope) =
    fn(args, scope)
}


case class Definition(name: Id, value: Expression) extends Statement {
  def asExpression(scope: Scope, modules: Modules) =
    value
}

case class Import(name: Id, exposing: List[Id]) extends Statement {
  def asExpression(scope: Scope, modules: Modules) =
    modules.get(name.lexeme) match {
      case None => Bool(false)
      case Some(module) =>
        val fields = module.scope.foldLeft[Map[Id, Expression]](Map()) {
          case (acc, (name, value)) if exposing.isEmpty || exposing.contains(name) =>
            acc ++ Map(name -> value)
          case (acc, _) => acc
        }
        Record(fields)
    }
}
