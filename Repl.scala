package bisquit
package repl

import ast._
import parser._
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
    var scope: RuntimeScope,
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

  def run(): Unit =
    while (true) {
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
          out.println("")
        }

      case (Mode.Ir, code) =>
        parseIt(code) {
          case expr: Expression =>
            out.println(pass1(expr))
            out.println("")
          case stmt: Statement =>
            out.println(stmt)
            out.println("")
        }

      case (Mode.Type, code) =>
        parseIt(code) {
          case expr: Expression =>
            typeIt(expr) { (_, ty) =>
              out.println(s": ${formatted(ty)}")
              out.println("")
            }
          case stmt: Statement =>
            out.println(s": ${formatted(UnitType)}")
            out.println("")
        }

      case (Mode.Eval, code) =>
        parseIt(code) {
          case expr: Expression =>
            typeIt(expr) { (_, ty) =>
              evalIt(expr) { value =>
                out.println(s"= ${formatted(value, lvl = 3, short = true)} : ${formatted(ty)}")
                out.println("")
              }
            }
          case stmt: Statement =>
            typeIt(stmt.asExpression) { (_, ty) =>
              doIt(stmt) {
                out.println(s"= ${formatted(stmt.asExpression, lvl = 3, short = true)} : ${formatted(ty)}")
                out.println("")
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
          out.println("")
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
        out.println("")
    }
  }

  def evalIt(expr: Expression)(ok: Value => Unit) =
    val ir = pass1(expr)
    eval(ir, scope) match {
      case Right(value) => ok(value)

      case Left(err) =>
        out.println(s"runtime error: $err")
        out.println("")
    }

  def doIt(stmt: Statement)(ok: Unit) =
    eval(stmt, scope) match {
      case Right(newScope) =>
        scope = newScope
      case Left(err) =>
        out.println(s"runtime error: $err")
        out.println("")
    }

  def mode(code: String): (Mode, String) =
    (code.split(" ").toList) match {
      case (":parse" :: rest) => (Mode.Parse, rest.mkString(" "))
      case (":type" :: rest) => (Mode.Type, rest.mkString(" "))
      case (":ir" :: rest) => (Mode.Ir, rest.mkString(" "))
      case _ => (Mode.Eval, code)
    }
}
