package bisquit

import java.io.{BufferedReader, InputStreamReader};

object Main {
  def main(args: Array[String]): Unit = {
    val input = new InputStreamReader(System.in)
    val reader = new BufferedReader(input)

    while (true) {
      print("> ")

      reader.readLine() match {
        case "exit" => return
        case code   => run(code)
      }
    }
  }

  def run(source: String) =
    println(Lexer.lex(source, "cli").toList)
}
