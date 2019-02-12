package xyz.minond.bisquit.node

object Printer {
  def error(err: Error) =
    err match {
      case UnknownToken(lexeme, _, _) =>
        s"error: invalid token `${lexeme}` in ${position(err)}"
      case UnexpectedToken(lexeme, msg, _, _) =>
        s"error: unexpected token `${lexeme}`, ${msg} in ${position(err)}"
      case _: UnexpectedEOF =>
        s"error: unexpected end of input in ${position(err)}"
      case UnexpectedExpr(token, msg) =>
        s"error: unexpected ${token}, expecting ${msg} in ${position(err)}"
      case InvalidExpr(UnknownToken(lexeme, _, _), None) =>
        s"error: invalid token `${lexeme}` in ${position(err)}"
      case InvalidExpr(got, None) =>
        s"error: invalid expression `${got}` in ${position(err)}"
      case InvalidExpr(
          Identifier(got, _, _),
          Some(Identifier(expected, _, _))
          ) =>
        s"error: invalid expression `${got}` in ${position(err)}, expected `${expected}`"
      case InvalidExpr(got, Some(expected)) =>
        s"error: invalid expression `${got}` in ${position(err)}, expected `${expected}`"
    }

  def position(err: Positioned) =
    s"${err.getFile}:${err.getStart}"
}
