package xyz.minond.bisquit.printer

import xyz.minond.bisquit.token._

def formatted(exprs: List[Expression], sep: String = " "): String =
  exprs.map(formatted(_)).mkString(sep)

def formatted(expr: Expression): String =
  expr match {
    case Id(lexeme) => lexeme
    case Binop(Id(op), left, right) => s"${formatted(left)} $op ${formatted(right)}"
    case Uniop(Id(op), right) => s"${op}${formatted(right)}"
    case App(Id(func), args) => s"${func}(${formatted(args, ", ")})"
    case App(fn : Func, args) => s"(${formatted(fn)})(${formatted(args, ", ")})"
    case Num(num) => num.toString
    case Func(params, body, _) => s"\\${formatted(params, " ")} -> ${formatted(body)}"
  }
