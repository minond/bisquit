package bisquit
package repl

import parser._
import printer._
import runtime._
import scope._
import typechecker._

import java.io._

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
      print(if (buff.isEmpty) promptStart else promptCont)

      buff.append(reader.readLine()).toString match {
        case "" =>
        case "exit" => return
        case code =>
          if process(code)
          then buff.clear
          else buff.append("\n")
      }
    }

  def process(code: String): Boolean =
    for res <- parse(code, fileName) do
      res match {
        case Left(_: UnexpectedEOF) =>
          return false
        case Left(err) =>
          out.println(s"parse error: $err")
          out.println("")
        case Right(expr) =>
          val ir = pass1(expr)
          infer(ir, scope, subs) match {
            case Left(err) =>
              out.println(s"type error: $err")
              out.println("")
            case Right(ty) =>
              eval(ir, scope) match {
                case Left(err) =>
                  out.println(s"runtime error: $err")
                  out.println("")
                case Right(value) =>
                  out.println(s"= ${formatted(value, lvl = 3, short = true)} : ${formatted(ty)}")
                  out.println("")
              }
          }
      }

    return true
}
