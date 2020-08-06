package xyz.minond.bisquit.printer

import xyz.minond.bisquit.token._

def formatted(exprs: List[Expression], lvl: Integer, sep: String = " "): String =
  exprs.map(formatted(_, lvl)).mkString(sep)

def formatted(expr: Expression): String =
  formatted(expr, 1)

def formatted(expr: Expression, lvl: Integer): String =
  expr match {
    case Id(lexeme) => lexeme
    case Binop(Id(op), left, right) => s"${formatted(left, lvl + 1)} $op ${formatted(right, lvl + 1)}"
    case Uniop(Id(op), right) => s"${op}${formatted(right, lvl + 1)}"
    case App(Id(func), args) => s"${func}(${formatted(args, lvl + 1, ", ")})"
    case App(fn : Func, args) => s"(${formatted(fn, lvl + 1)})(${formatted(args, lvl + 1, ", ")})"
    case Bool(v) => if (v) "#t" else "#f"
    case Num(num) if num < 0 => s"~${Math.abs(num)}"
    case Num(num) => num.toString
    case Cons(Nil) => "[]"
    case Cons(values) =>
      val indent = " " * (lvl - 1)
      s"[ ${formatted(values, lvl + 1, s"\n$indent, ")}\n$indent]"
    case Func(params, body) =>
      val indent = " " * (lvl + 1)
      val spacing = if (params.isEmpty) "" else " "
      s"\\${formatted(params, lvl + 1, " ")}$spacing->\n${indent}${formatted(body, lvl + 1)}"
    case _: Builtin => "<builtin>"
    case Let(bindings, body) =>
      val names = bindings.keys
      val values = bindings.values.map { formatted(_, lvl + 2 + 4) }
      val argIndent = " " * (lvl + 1)
      val indent = " " * (lvl - 1)
      val decls = names.zip(values).map { (n, v) => s"\n${argIndent}$n = $v" }.mkString
      s"let${decls}\n${indent}in ${formatted(body, lvl + 1)}"
  }
