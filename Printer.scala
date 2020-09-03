package bisquit
package printer

import scala.collection.mutable.{Map => MMap}

import ast.{Int => _, _}
import typechecker._


case class Labeler(labels: MMap[Int, String] = MMap()) {
  private val nums = LazyList.from(97).sliding(1)

  def apply(id: Int): String =
    labels.get(id) match {
      case Some(label) => label
      case None =>
        val label = nums.next.head.toChar.toString
        labels.addOne(id, label)
        label
    }
}


def formatted(exprs: List[Expression], lvl: Int, sep: String = " "): String =
  exprs.map(formatted(_, lvl)).mkString(sep)

def formatted(expr: Expression): String =
  formatted(expr, 1)

def formatted(expr: Expression, lvl: Int): String =
  expr match {
    case Id(lexeme) => lexeme
    case Binop(Id(op), left, right) => s"${formatted(left, lvl + 1)} $op ${formatted(right, lvl + 1)}"
    case Uniop(Id(op), right) => s"${op}${formatted(right, lvl + 1)}"
    case App(Id(func), args) => s"${func}(${formatted(args, lvl + 1, ", ")})"
    case App(fn, args) =>
      val body = formatted(fn, lvl + 1)
      val argLvl = body.split("\n").last.size
      s"(${body})(${formatted(args, argLvl + 3, ", ")})"
    case Bool(v) => if v then "#t" else "#f"
    case ast.Int(num) if num < 0 => s"~${Math.abs(num)}"
    case ast.Int(num) => num.toString
    case Str(str) => s""""$str""""
    case Lambda(params, body, _) =>
      val indent = " " * lvl
      val spacing = if params.isEmpty then "" else " "
      val sig = if params.isEmpty
                then ""
                else s"(${formatted(params, lvl + 1, ", ")})"
      s"\\$sig- ${formatted(body, lvl + 2)}"
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
      val header = if lvl > 1
                   then s"\n$indent"
                   else ""
      s"${header}if ${scond}\n${indent}then ${spass}\n${indent}else ${sfail}"
  }

def formatted(ty: Type): String =
  formatted(ty, Labeler(), false)

def formatted(ty: Type, label: Labeler, nested: Boolean): String =
  ty match {
    case UnitType => "Unit"
    case IntType => "Int"
    case StrType => "Str"
    case BoolType => "Bool"
    case LambdaType(tys) =>
      val s = tys.map(formatted(_, label, true)).mkString(" -> ")
      if nested
      then s"($s)"
      else s
    case TypeVariable(id) => label(id)
  }
