package bisquit

object Main {
  def main(args: Array[String]): Unit =
    println(Lexer.lex("""

toString(n : int) : string =
  n match
    | 0 => "zero"
    | 1 => "one"
    | 2 => "two"
    | 3 => "three"
    | 4 => "four"
    | _ => "???"

yes : string =
  if true
  then "Yes"
  else "No"

      """).toList)
}
