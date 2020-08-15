package bisquit
package printer

import ast._
import typechecker._

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
    case App(fn, args) => s"(${formatted(fn, lvl + 1)})(${formatted(args, lvl + 1, ", ")})"
    case Bool(v) => if v then "#t" else "#f"
    case Int(num) if num < 0 => s"~${Math.abs(num)}"
    case Int(num) => num.toString
    case Str(str) => str
    case Lambda(params, body, _) =>
      val indent = " " * (lvl + 1)
      val spacing = if params.isEmpty then "" else " "
      s"\\${formatted(params, lvl + 1, " ")}$spacing->\n${indent}${formatted(body, lvl + 1)}"
    case _: Builtin => "<builtin>"
    case Let(bindings, body) =>
      val names = bindings.keys
      val values = bindings.values.map { formatted(_, lvl + 2 + 4) }
      val argIndent = " " * (lvl + 1)
      val indent = " " * (lvl - 1)
      val decls = names.zip(values).map { (n, v) => s"\n${argIndent}$n = $v" }.mkString
      s"let${decls}\n${indent}in ${formatted(body, lvl + 3)}"
    case Cond(cond, pass, fail) =>
      val indent = " " * (lvl - 1)
      val scond = formatted(cond, lvl + 1)
      val spass = formatted(pass, lvl + 1)
      val sfail = formatted(fail, lvl + 1)
      s"if ${scond}\n${indent}then ${spass}\n${indent}else ${sfail}"
  }

def formatted(ty: Type): String =
  ty match {
    case UnitType => "unit"
    case IntType => "int"
    case StrType => "str"
    case BoolType => "bool"
    case LambdaType(tys) => tys.map(formatted(_)).mkString(" -> ")
  }
