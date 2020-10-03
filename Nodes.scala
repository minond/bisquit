package bisquit
package nodes

import errors._
import input._
import runtime._
import scope._
import typechecker._


sealed trait Token extends Positioned
case class Eof() extends Token
case class Comma() extends Token
case class OpenParen() extends Token
case class CloseParen() extends Token
case class OpenCurlyBraket() extends Token
case class CloseCurlyBraket() extends Token
case class OpenSquareBraket() extends Token
case class CloseSquareBraket() extends Token
case class Colon() extends Token
case class Equal() extends Token
case class Dot() extends Token


sealed trait Expression extends Positioned with Typing
case class Binop(op: Expression, left: Expression, right: Expression) extends Expression
case class Uniop(op: Expression, subject: Expression) extends Expression


sealed trait IR extends Typing with Positioned
case class Id(lexeme: String) extends IR with Expression with Token
case class App(fn: Expression, args: List[Expression]) extends IR with Expression
case class Let(bindings: Map[Id, Expression], body: Expression) extends IR with Expression
case class Cond(cond: Expression, pass: Expression, fail: Expression) extends IR with Expression
case class RecordLookup(rec: Expression, field: Id) extends IR with Expression


sealed trait Value extends Expression
case class Int(value: Integer) extends IR with Value with Token
case class Real(value: Double) extends IR with Value with Token
case class Str(value: String) extends IR with Value with Token
case class Bool(value: Boolean) extends IR with Value with Token
case class Tuple(fields: List[Expression]) extends IR with Value
case class Lista(items: List[Expression]) extends IR with Value
case class RefCell(var value: Value) extends IR with Value
case class Record(fields: Map[Id, Expression] = Map()) extends IR with Value


trait Callable {
  def apply(args: List[IR], scope: Scope):
    Either[RuntimeError, Value]
}

type BuiltinFunc = (List[IR], Scope) => Either[RuntimeError, Value]

case class Builtin(sig: LambdaType, fn: BuiltinFunc)
    extends IR with Value with Typed(sig) with Callable {
  def apply(args: List[IR], scope: Scope) =
    fn(args, scope)
}

case class Lambda(
  params: List[Id],
  body: Expression,
  boundScope: Option[Scope],
) extends IR with Value with Callable {
  def apply(args: List[IR], scope: Scope): Either[RuntimeError, Value] =
    for
      vals <- eval(args, scope)
      ret <- evalIt(vals, scope)
    yield ret

  def evalIt(vals: List[Value], scope: Scope) =
    val argScope = params.zip(vals).toMap
    val lexScope = scope ++ boundScope.getOrElse(Map()) ++ argScope
    if params.size != vals.size
    then Right(curryIt(vals, lexScope))
    else eval(pass1(body), lexScope)

  def curryIt(bindings: List[Value], lexScope: Scope) =
    Lambda(params = params.drop(bindings.size),
           body = App(fn = Lambda(params = params.take(bindings.size),
                                  body = body,
                                  boundScope = boundScope),
                      args = bindings),
           boundScope = Some(lexScope))
}


sealed trait Statement
case class Definition(name: Id, value: Expression) extends Statement
case class Import(name: Id, exposing: List[Id], all: Boolean = false) extends Statement
case class Module(name: Id, exposes: Set[Id], scope: Scope) extends Statement {
  def expose(
      lexScope: Scope,
      modules: Modules,
      exposing: List[Id],
      all: Boolean,
  ): Either[RuntimeError, Scope] =
    (
      exposing.isEmpty,
      all,
      exposing.diff(exposing.distinct).distinct,
      exposing.diff(exposes.toList),
    ) match {
      case (_, true, _, _) =>
        val fields = scope.filter { (name, _) =>
          exposes.contains(name)
        }

        Right(lexScope ++ fields)

      case (true, false, _, _) =>
        val fields = scope.filter { (name, _) => exposes.contains(name) }
        val record = Record(fields)
        Right(lexScope ++ Map(name -> record))

      case (_, _, Nil, Nil) =>
        val fields = scope.filter { (name, _) =>
          exposing.contains(name) && exposes.contains(name)
        }

        Right(lexScope ++ fields)

      case (_, _, Nil, missing) =>
        Left(ModuleValueNotExposed(missing.head, this))

      case (_, _, dups, _) =>
        Left(DuplicateExposeName(dups.head))
    }
}
