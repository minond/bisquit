package xyz.minond.bisquit

import java.io.{BufferedReader, InputStreamReader};

import xyz.minond.bisquit.node._

object Main {
  def main(args: Array[String]): Unit = {
    val input = new InputStreamReader(System.in)
    val reader = new BufferedReader(input)
    val buff = new StringBuilder

    while (true) {
      if (buff.isEmpty)
        print("> ")
      else
        print("  ")

      buff.append(reader.readLine()).toString match {
        case ""     =>
        case "exit" => return
        case code =>
          Parser.parse(Lexer.lex(code, "stdin")).toList match {
            case (_: UnexpectedEOF) :: Nil =>
              buff.append("\n")

            case ast =>
              buff.clear
              println(ast)
          }
      }
    }
  }
}
