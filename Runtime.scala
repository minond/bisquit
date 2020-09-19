package bisquit
package runtime

import scala.language.implicitConversions
import scala.reflect.ClassTag

import ast._
import scope._
import utils.{ensure, remap, rekey, formap}
import utils.Implicits.Eithers

sealed trait RuntimeError
case class LookupError(id: Id) extends RuntimeError
case class ArgumentTypeError(arg: IR) extends RuntimeError
case class ConditionError(cond: IR) extends RuntimeError
case class RecordLookupError(id: Id, record: Record) extends RuntimeError
case class ExpectedRecordInstead(got: Value) extends RuntimeError
case class UnexportedModuleValue(name: String, module: Module) extends RuntimeError


def pass1(expr: Expression): IR with Expression =
  expr match {
    case Uniop(op, a) => App(op, List(pass1(a)))
    case Binop(op, l, r) => App(op, List(pass1(l), pass1(r)))
    case Lambda(params, body, scope) => Lambda(params, pass1(body), scope)
    case App(fn, args) => App(pass1(fn), args.map(pass1))
    case Cond(cond, pass, fail) => Cond(pass1(cond), pass1(pass), pass1(fail))
    case Let(bindings, body) => Let(remap(bindings) { pass1 }, pass1(body))
    case Record(fields) => Record(remap(fields) { pass1 })
    case ir: IR => ir
  }


def eval(stmt: Statement, scope: Scope, modules: Modules): Either[RuntimeError, (Scope, Modules)] =
  stmt match {
    case Definition(name, value) =>
      for
        evaled <- eval(pass1(value), scope)
      yield
        (scope ++ Map(name.lexeme -> evaled), modules)

    case Import(name, exposing) =>
      modules.get(name.lexeme) match {
        case None => /** TODO import module */ ???
        case Some(module) =>
          if exposing.isEmpty
          then
            val fields = rekey(module.scope) { Id(_) }
            val record = Record(fields)
            Right((scope ++ Map(name.lexeme -> record), modules))
          else
            val needs = exposing.map(_.lexeme)
            val missing = needs.diff(module.scope.keys.toList)

            if !missing.isEmpty
            then Left(UnexportedModuleValue(missing.head, module))
            else
              val fields = module.scope.foldLeft[Map[String, Value]](Map()) {
                case (acc, (name, value)) if needs.contains(name) =>
                  acc ++ Map(name -> value)
                case (acc, _) => acc
              }

              Right((scope ++ fields, modules))
      }
  }

def eval(exprs: List[IR]): Either[RuntimeError, List[Value]] =
  eval(exprs, Map())

def eval(expr: IR): Either[RuntimeError, Value] =
  eval(expr, Map())

def eval(exprs: List[IR], scope: Scope): Either[RuntimeError, List[Value]] =
  exprs.map { eval(_, scope) }.squished()

def eval(expr: IR, scope: Scope): Either[RuntimeError, Value] =
  expr match {
    case Lambda(args, body, _) => Right(Lambda(args, body, scope))
    case Tuple(fields) => evalTuple(fields, scope)
    case Record(fields) => evalRecord(fields, scope)
    case RecordLookup(rec, field) => evalRecordLookup(rec, field, scope)
    case value: Value => Right(value)
    case id: Id => lookup(id, scope)
    case App(fn, args) => evalCallable(pass1(fn), args.map(pass1), scope)
    case Let(bindings, body) => evalLet(bindings, body, scope)
    case Cond(cond, pass, fail) => evalCond(cond, pass, fail, scope)
  }

def evalTuple(fields: List[Expression], scope: Scope) =
  for
    inners <- fields.map { v => eval(pass1(v), scope) }.squished()
  yield Tuple(inners)

def evalRecordLookup(rec: Expression, field: Id, scope: Scope) =
  for
    maybeRecord <- eval(pass1(rec), scope)
    record <- ensure[RuntimeError, Record](maybeRecord, ExpectedRecordInstead(maybeRecord))
    expr <- lookup(field, record.fields, RecordLookupError(field, record))
    value <- eval(pass1(expr), scope)
  yield value

def evalRecord(fields: Map[Id, Expression], scope: Scope) =
  for
    inners <- formap(fields){ v => eval(pass1(v), scope) }
    ret = Record(inners)
  yield ret

def evalCallable(fn: IR, args: List[IR], scope: Scope) =
  for
    maybeCallable <- eval(fn, scope)
    callable <- ensure[RuntimeError, Callable](maybeCallable, ArgumentTypeError(fn))
    ret <- callable.apply(args, scope)
  yield ret

def evalLet(bindings: Map[Id, Expression], body: Expression, scope: Scope) =
  for
    bound <- letRec(bindings, scope)
    ret <- eval(pass1(body), bound)
  yield ret

def evalCond(cond: Expression, pass: Expression, fail: Expression, scope: Scope) =
  for
    res <- eval(pass1(cond), scope)
    bool <- ensure[RuntimeError, Bool](res, ConditionError(pass1(cond)))
    body = if bool.value then pass else fail
    ret <- eval(pass1(body), scope)
  yield ret


def letRec(bindings: Map[Id, Expression], scope: Scope) =
  bindings.foldLeft[Either[RuntimeError, Scope]](Right(scope)) {
    case (acc, (id, expr)) =>
      acc.flatMap { recscope =>
        eval(pass1(expr), recscope).map { v =>
          recscope ++ Map(id.lexeme -> v)
        }
      }
  }


def lookup[V, L](id: Id, scope: Map[Id, V], left: => L): Either[L, V] =
  scope.get(id) match {
    case None => Left(left)
    case Some(value) => Right(value)
  }

def lookup(id: Id, scope: Scope): Either[LookupError, Value] =
  scope.get(id.lexeme) match {
    case None => Left(LookupError(id))
    case Some(value) => Right(value)
  }
