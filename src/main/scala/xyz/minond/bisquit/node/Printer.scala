package xyz.minond.bisquit.node

object Printer {
  def error(err: Error) =
    err match {
      case _: UnexpectedEOF =>
        s"error: unexpected end of input in ${position(err)}"
      case UnexpectedExpr(token, msg) =>
        s"error: unexpected ${token}, expecting ${msg} in ${position(err)}"
      case InvalidExpr(InvalidToken(lexeme, _, _), None) =>
        s"error: invalid token `${lexeme}` in ${position(err)}"
      case InvalidExpr(got, None) =>
        s"error: invalid expression `${got}` in ${position(err)}"
      case InvalidExpr(got, Some(expected)) =>
        s"error: invalid expression `${got}`, expected `${expected} in ${position(err)}"
    }

  def position(err: Positioned) =
    s"${err.getFile}:${err.getStart}"
}
