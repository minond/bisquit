package xyz.minond.bisquit

import java.io.{BufferedReader, InputStreamReader}
import scala.util.{Failure, Success, Try}

object Repl {
  val promptPrefix = ""
  val promptStart = s"${promptPrefix}> "
  val promptCont = s"${" " * promptPrefix.size}| "

  def main(args: Array[String]): Unit = {
    val input = new InputStreamReader(System.in)
    val reader = new BufferedReader(input)
    val buff = new StringBuilder

    var env = TypeEnvironment.create

    while (true) {
      print(if (buff.isEmpty) promptStart else promptCont)

      buff.append(reader.readLine()).toString match {
        case ""     =>
        case "exit" => return
        case code =>
          Parser.process(code, "<stdin>").toList match {
            case Left(_: UnexpectedEOF) :: Nil =>
              buff.append("\n")

            case ast =>
              buff.clear
              ast.foreach {
                _.fold(
                  err => println(err),
                  exp => {
                    Try { Ty.process(exp, env) } match {
                      case Failure(err) =>
                        println(s"exception while typing expressiong: $err")
                        println(s"ast: $exp")
                      case Success(Left(err)) =>
                        println(err)
                        println(s"ast: $exp")
                      case Success(Right((ty, _env))) =>
                        println(s"t : $ty")
                        env = _env
                    }
                  }
                )
              }
          }
      }
    }
  }
}
