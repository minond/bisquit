package xyz.minond.bisquit

import java.io.{BufferedReader, InputStreamReader};

import xyz.minond.bisquit.node._

object Main {
  val promptPrefix = "bisquit"
  val promptStart = s"${promptPrefix}> "
  val promptCont = s"${" " * promptPrefix.size}| "

  def main(args: Array[String]): Unit = {
    val input = new InputStreamReader(System.in)
    val reader = new BufferedReader(input)
    val buff = new StringBuilder

    while (true) {
      print(if (buff.isEmpty) promptStart else promptCont)

      buff.append(reader.readLine()).toString match {
        case ""     =>
        case "exit" => return
        case code =>
          Parser.parse(Lexer.lex(code, "<stdin>")).toList match {
            case Left(_: UnexpectedEOF) :: Nil =>
              buff.append("\n")

            case ast =>
              buff.clear
              ast.foreach {
                _.fold(
                  err => println(Printer.error(err)),
                  ok => println(ok)
                )
              }
          }
      }
    }
  }
}
