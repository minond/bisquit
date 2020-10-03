package bisquit
package repl

import errors._
import nodes._
import parser._
import prelude._
import printer._
import runtime._
import scope._
import typechecker._

import java.io._
import scala.sys.process._
import scala.language.postfixOps
import scala.language.implicitConversions


enum Mode {
  case Parse
  case Ir
  case Type
  case Eval
}


class Repl(
    var scope: Scope = Map(),
    in: InputStream = System.in,
    out: PrintStream = System.out,
) {
  val promptPrefix = ""
  val promptStart = s"${promptPrefix}> "
  val promptCont = s"${" " * promptPrefix.size}| "

  val input = new InputStreamReader(in)
  val reader = new BufferedReader(input)
  val buff = new StringBuilder
  val fileName = "<repl>"

  var modules: Modules = Prelude
  var newLine: Boolean = false

  val version = ("git log -1 --format='%H'" !!).trim()
  val initCommand = "import Prelude exposing (...)"

  def run(): Unit =
    out.println(s"Bisquit version $version")
    out.println("Run 'exit' to exit.")
    out.println(s"\n$promptStart$initCommand")
    process(initCommand)

    while (true) {
      if newLine
      then out.print("\n")

      newLine = false

      if buff.isEmpty
      then out.print(promptStart)

      buff.append(reader.readLine()).toString match {
        case "" =>
        case "exit" => return
        case code =>
          if process(code)
          then buff.clear
          else buff.append("\n")
      }
    }

  def process(origCode: String): Boolean =
    mode(origCode) match {
      case (Mode.Parse, code) =>
        parseIt(code) { expr =>
          out.println(expr)
          newLine = true
        }

      case (Mode.Ir, code) =>
        parseIt(code) {
          case expr: Expression =>
            out.println(pass1(expr))
            newLine = true
          case stmt: Statement =>
            out.println(stmt)
            newLine = true
        }

      case (Mode.Type, code) =>
        parseIt(code) {
          case expr: Expression =>
            typeIt(expr, code) { (_, ty) =>
              out.println(s": ${formatted(ty)}")
              newLine = true
            }
          case stmt: Statement =>
            typeIt(stmt, code) { (_, ty) =>
              out.println(s": ${formatted(ty)}")
              newLine = true
            }
        }

      case (Mode.Eval, code) =>
        parseIt(code) {
          case expr: Expression =>
            typeIt(expr, code) { (_, ty) =>
              evalIt(expr, code) { value =>
                out.println(s"= ${formatted(value, lvl = 3, short = true)} : ${formatted(ty)}")
                newLine = true
              }
            }
          case stmt: Statement =>
            stmt match {
              case module: Module =>
                doIt(module, code) {
                  out.println("< ok")
                  newLine = true
                }

              case ymport @ Import(name, _, _) if name == InternalModuleName =>
                doIt(ymport, code) {
                  out.println("< ok")
                  newLine = true
                }

              case ymport @ Import(name, _, _) =>
                doIt(ymport, code, modules.removed(name)) {
                  out.println("< ok")
                  newLine = true
                }

              case stmt =>
                typeIt(stmt, code) { (_, ty) =>
                  doIt(stmt, code) {
                    out.println(s": ${formatted(ty)}")
                    newLine = true
                  }
                }
            }
        }
    }

  def parseIt(code: String)(ok: (Expression | Statement) => Unit): Boolean = {
    for res <- parse(code, fileName) do
      res match {
        case Right(expr) => ok(expr)

        case Left(_: UnexpectedEOF) =>
          return false

        case Left(err) =>
          out.println(s"${errorType(err)}: ${formatted(err, code)}")
          newLine = true
          return true
      }

    true
  }

  def typeIt(stmt: Statement, code: String)(ok: (Expression, Type) => Unit) = {
    infer(stmt, scope, Substitution()) match {
      case Right(ty) =>
        stmt match {
          case Definition(_, value) => ok(value, ty)
          case _ => ok(Bool(true), BoolType())
        }

      case Left(err) =>
        out.println(s"${errorType(err)}: ${formatted(err, code)}")
        newLine = true
    }
  }

  def typeIt(expr: Expression, code: String)(ok: (Expression, Type) => Unit) = {
    val ir = pass1(expr)
    infer(ir, scope, Substitution()) match {
      case Right(ty) => ok(expr, ty)

      case Left(err) =>
        out.println(s"${errorType(err)}: ${formatted(err, code)}")
        newLine = true
    }
  }

  def evalIt(expr: Expression, code: String)(ok: Value => Unit) =
    val ir = pass1(expr)
    eval(ir, scope) match {
      case Right(value) => ok(value)

      case Left(err) =>
        out.println(s"${errorType(err)}: ${formatted(err, code)}")
        newLine = true
    }

  def doIt(stmt: Statement, code: String, currModules: Modules = modules)(ok: => Unit) =
    eval(stmt, scope, currModules) match {
      case Right((newScope, newModules)) =>
        scope = newScope
        modules = newModules
        ok
      case Left(err) =>
        out.println(s"${errorType(err)}: ${formatted(err, code)}")
        newLine = true
    }

  def mode(code: String): (Mode, String) =
    (code.split(" ").toList) match {
      case (":parse" :: rest) => (Mode.Parse, rest.mkString(" "))
      case (":type" :: rest) => (Mode.Type, rest.mkString(" "))
      case (":ir" :: rest) => (Mode.Ir, rest.mkString(" "))
      case _ => (Mode.Eval, code)
    }
}
