package bisquit
package input

import nodes.Token


case class Position(file: String, offset: Int) {
  override def toString =
    s"$file:$offset"
}

trait Positioned { self =>
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

def getSurroundingLines(offset: Int, source: String, count: Int): Option[(List[String], List[Int], Int, Int)] =
  val lines = source.split("\n").toList
  getRowColByOffset(offset, lines) match {
    case None => None
    case Some((row, col)) =>
      val start = if row - count < 0
                  then 0
                  else row - count

      val end = if row + count >= lines.size
                then lines.size
                else row + count

      Some(lines.slice(start, end), (start until end).toList, row, col)
  }

def getRowColByOffset(offset: Int, source: String): Option[(Int, Int)] =
  getRowColByOffset(offset, source.split("\n").toList)

def getRowColByOffset(offset: Int, lines: List[String]): Option[(Int, Int)] =
  var curr = 0
  var col = 0
  for (contents, row) <- lines.zipWithIndex do
    col = offset - curr - row
    curr += contents.size
    if curr >= offset
    then return Some((row, col))
  None
