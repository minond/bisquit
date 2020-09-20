package bisquit
package repl

import ast._
import parser._
import prelude._
import printer._
import runtime._
import scope._
import typechecker._

import java.io._


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

  val subs = Substitution()

  var modules: Modules = Prelude

  def run(): Unit =
    while (true) {
      out.println("")
      out.print(if (buff.isEmpty) promptStart else promptCont)

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
        }

      case (Mode.Ir, code) =>
        parseIt(code) {
          case expr: Expression =>
            out.println(pass1(expr))
          case stmt: Statement =>
            out.println(stmt)
        }

      case (Mode.Type, code) =>
        parseIt(code) {
          case expr: Expression =>
            typeIt(expr) { (_, ty) =>
              out.println(s": ${formatted(ty)}")
            }
          case stmt: Statement =>
            typeIt(stmt.asExpression(scope, modules)) { (_, ty) =>
              out.println(s": ${formatted(ty)}")
            }
        }

      case (Mode.Eval, code) =>
        parseIt(code) {
          case expr: Expression =>
            typeIt(expr) { (_, ty) =>
              evalIt(expr) { value =>
                out.println(s"= ${formatted(value, lvl = 3, short = true)} : ${formatted(ty)}")
              }
            }
          case stmt: Statement =>
            stmt match {
              case module: Module =>
                doIt(module) {
                  out.println("< ok")
                }

              case ymport: Import =>
                doIt(ymport) {
                  out.println("< ok")
                }

              case stmt =>
                typeIt(stmt.asExpression(scope, modules)) { (_, ty) =>
                  doIt(stmt) {
                    out.println(s": ${formatted(ty)}")
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
          out.println(s"parse error: $err")
          return true
      }

    true
  }

  def typeIt(expr: Expression)(ok: (Expression, Type) => Unit) = {
    val ir = pass1(expr)
    infer(ir, scope, subs) match {
      case Right(ty) => ok(expr, ty)

      case Left(err) =>
        out.println(s"type error: $err")
    }
  }

  def evalIt(expr: Expression)(ok: Value => Unit) =
    val ir = pass1(expr)
    eval(ir, scope) match {
      case Right(value) => ok(value)

      case Left(err) =>
        out.println(s"runtime error: $err")
    }

  def doIt(stmt: Statement)(ok: => Unit) =
    eval(stmt, scope, modules) match {
      case Right((newScope, newModules)) =>
        scope = newScope
        modules = newModules
        ok
      case Left(err) =>
        out.println(s"runtime error: $err")
    }

  def mode(code: String): (Mode, String) =
    (code.split(" ").toList) match {
      case (":parse" :: rest) => (Mode.Parse, rest.mkString(" "))
      case (":type" :: rest) => (Mode.Type, rest.mkString(" "))
      case (":ir" :: rest) => (Mode.Ir, rest.mkString(" "))
      case _ => (Mode.Eval, code)
    }
}
