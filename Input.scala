package xyz.minond.bisquit.input

import xyz.minond.bisquit.token.Token

case class Position(file: String, line: Integer, column: Integer) {
  override def toString =
    s"$file:$column:$line"
}

open class Positioned {
  var position: Option[Position] = None

  def at(position: Position) = {
    this.position = Some(position)
    this
  }
}

object Positioned {
  val stdin = file("<stdin>")
  val input = file("<input>")

  def file(file: String) =
    (token: Token, line: Integer, column: Integer) =>
      token.at(Position(file, line, column))
}
