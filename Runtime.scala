package bisquit
package runtime

import scala.language.implicitConversions
import scala.reflect.ClassTag

import errors._
import nodes._
import parser._
import prelude._
import scope._
import typechecker._

import utils.{ensure, remap, formap}
import utils.Implicits.Eithers

import java.io.File
import java.util.Scanner


val DefaultLoadPath = List("lib", ".")


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


def eval(stmt: Statement, scope: Scope, modules: Modules): Either[BisquitError, (Scope, Modules)] =
  stmt match {
    case Definition(name, value) =>
      for
        evaled <- eval(pass1(value), scope)
      yield
        (scope ++ Map(name -> evaled), modules)

    case Import(name, exposing, all) =>
      modules.get(name) match {
        case None =>
          for
            file <- findModuleFile(name.lexeme, DefaultLoadPath)
            ret <- load(file, modules)
            (module, nextModules) = ret
            nextScope <- module.expose(scope, nextModules, exposing, all)
          yield
            (nextScope, nextModules)

        case Some(module) =>
          for
            nextScope <- module.expose(scope, modules, exposing, all)
          yield
            (nextScope, modules)
      }

    case Module(name, _, _) =>
      modules.get(name) match {
        case None => Right((Map(), modules))
        case Some(Module(_, _, newScope)) => Right((newScope, modules))
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
    case Lambda(args, body, None) => Right(Lambda(args, body, Some(scope)))
    case Lambda(args, body, Some(scope)) => Right(Lambda(args, body, Some(scope)))
    case Lista(fields) => evalLista(fields, scope)
    case Tuple(fields) => evalTuple(fields, scope)
    case Record(fields) => evalRecord(fields, scope)
    case RecordLookup(rec, field) => evalRecordLookup(rec, field, scope)
    case value: Value => Right(value)
    case id: Id => lookup(id, scope)
    case App(fn, args) => evalCallable(pass1(fn), args.map(pass1), scope)
    case Let(bindings, body) => evalLet(bindings, body, scope)
    case Cond(cond, pass, fail) => evalCond(cond, pass, fail, scope)
  }

def evalLista(items: List[Expression], scope: Scope) =
  for
    inners <- items.map { v => eval(pass1(v), scope) }.squished()
  yield Lista(inners)

def evalTuple(fields: List[Expression], scope: Scope) =
  for
    inners <- fields.map { v => eval(pass1(v), scope) }.squished()
  yield Tuple(inners)

def evalRecordLookup(rec: Expression, field: Id, scope: Scope) =
  for
    maybeRecord <- eval(pass1(rec), scope)
    record <- ensure[RuntimeError, Record](maybeRecord, RuntimeExpectedRecordInstead(maybeRecord))
    expr <- lookup(field, record.fields, RuntimeRecordLookupError(field, record))
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
          recscope ++ Map(id -> v)
        }
      }
  }


def lookup[V, L](id: Id, scope: Map[Id, V], left: => L): Either[L, V] =
  scope.get(id) match {
    case None => Left(left)
    case Some(value) => Right(value)
  }

def lookup(id: Id, scope: Scope): Either[RuntimeLookupError, Value] =
  scope.get(id) match {
    case None => Left(RuntimeLookupError(id))
    case Some(value) => Right(value)
  }


def moduleNameFromFileName(fileName: String): String =
  fileName.split("/").last.split("\\.").head

def fileNameFromModuleName(moduleName: String): String =
  moduleName.replaceAll("\\.", "/") + ".bisquit"

def findModuleFile(moduleName: String, loadPaths: List[String]): Either[BisquitError, File] =
  val name = fileNameFromModuleName(moduleName)
  for path <- loadPaths do
    val file = File(path, name)
    if file.isFile
    then return Right(file)
  Left(FileNotFound(name, loadPaths))

def load(file: File, currModules: Modules = Prelude): Either[BisquitError, (Module, Modules)] =
  val scanner = Scanner(file)
  val buffer = StringBuilder()
  val name = Id(moduleNameFromFileName(file.getName()))

  var scope: Scope = Map()
  var modules: Modules = currModules
  var exposing: Set[Id] = Set.empty

  while (scanner.hasNextLine()) {
    buffer.append(s"${scanner.nextLine()}\n")
  }

  for res <- parse(buffer.mkString, file.getName()) do
    res match {
      case Left(err) => return Left(err)

      case Right(expr: Expression) =>
        val ir = pass1(expr)
        infer(ir, scope, Substitution()) match {
          case Left(err) => return Left(err)
          case Right(_) =>
            eval(ir, scope) match {
              case Left(err) => return Left(err)
              case Right(_) =>
            }
        }

      case Right(stmt: Statement) =>
        stmt match {
          case Module(_name, _exposing, _) if name == _name =>
            exposing = _exposing

          case Module(badName, _, _) =>
            return Left(IncorrectModuleName(badName, name))

          case _ =>
            infer(stmt, scope, Substitution()) match {
              case Left(err) => return Left(err)
              case Right(ty) =>
                eval(stmt, scope, modules) match {
                  case Left(err) => return Left(err)
                  case Right((newScope, newModules)) =>
                    scope = newScope
                    modules = newModules
                }
            }
        }
    }

  val module = Module(name, exposing, scope)
  Right((module, modules ++ Map(name -> module)))
