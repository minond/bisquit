package bisquit
package input

import ast.Token

case class Position(file: String, offset: Int) {
  override def toString =
    s"$file:$offset"
}

open class Positioned { self =>
  var position: Option[Position] = None

  def at(position: Position): self.type =
    this.position = Some(position)
    this
}

class Positioner(val file: String) {
  def apply(token: Token, offset: Int) =
    token.at(Position(file, offset))

  def at(offset: Int) =
    Position(file, offset)
}

object Positioned {
  val stdin = file("<stdin>")
  val input = file("<input>")

  def file(file: String) =
    Positioner(file)
}
