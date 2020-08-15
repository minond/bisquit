package bisquit
package input

import ast.Token

case class Position(file: String, line: Integer, column: Integer) {
  override def toString =
    s"$file:$column:$line"
}

open class Positioned { self =>
  var position: Option[Position] = None

  def at(position: Position): self.type =
    this.position = Some(position)
    this
}

object Positioned {
  val stdin = file("<stdin>")
  val input = file("<input>")

  def file(file: String) =
    (token: Token, line: Integer, column: Integer) =>
      token.at(Position(file, line, column))
}
