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


def formatted(exprs: List[Expression], lvl: Int, nested: Boolean, sep: String = " "): String =
  exprs.map(formatted(_, lvl, nested)).mkString(sep)

def formatted(expr: Expression): String =
  formatted(expr, 1)

def formatted(expr: Expression, lvl: Int): String =
  formatted(expr, lvl, false)

def formatted(expr: Expression, lvl: Int, nested: Boolean): String =
  expr match {
    case id @ Id(lexeme) =>
      id.ty match {
        case None => lexeme
        case Some(ty) => s"${lexeme} : ${formatted(ty)}"
      }
    case Binop(op, left, right) => s"${formatted(left, lvl + 1, false)} ${formatted(op)} ${formatted(right, lvl + 1, false)}"
    case Uniop(op, right) => s"${formatted(op)}${formatted(right, lvl + 1, false)}"
    case App(Id(func), args) => s"${func}(${formatted(args, lvl + 1, false, ", ")})"
    case App(fn, args) =>
      val body = formatted(fn, lvl, false)
      val argLvl = body.split("\n").last.size
      val indent = " " * (lvl - 1)
      if nested
      then s"\n${indent}(${body})(${formatted(args, argLvl + 3, false, ", ")})"
      else s"(${body})(${formatted(args, argLvl + 3, false, ", ")})"
    case Bool(v) => if v then "#t" else "#f"
    case RecordLookup(rec, field) => s"${formatted(rec, lvl, false)}.${formatted(field, lvl, false)}"
    case Record(fields) =>
      val pairs = fields.map { (k, v) => s"${formatted(k, lvl, false)}: ${formatted(v, lvl, nested)}" }
      val indent = " " * (lvl - 1)
      s"{ ${pairs.mkString(s"\n${indent}, ")} }"
    case ast.Int(num) if num < 0 => s"~${Math.abs(num)}"
    case ast.Int(num) => num.toString
    case Str(str) => s""""$str""""
    case Lambda(params, body, _) =>
      val indent = " " * lvl
      val spacing = if params.isEmpty then "" else " "
      val sig = if params.isEmpty
                then ""
                else s"(${formatted(params, lvl + 1, false, ", ")})"
      if nested
      then s"\n${indent}\\$sig. ${formatted(body, lvl + 2, true)}"
      else s"\\$sig. ${formatted(body, lvl + 2, true)}"
    case _: Builtin => "<builtin>"
    case Let(bindings, body) =>
      val names = bindings.keys
      val values = bindings.values.map { formatted(_, lvl + 2 + 4, false) }
      val argIndent = " " * (lvl + 1)
      val indent = " " * (lvl - 1)
      val decls = names.zip(values).map { (n, v) => s"\n${argIndent}$n = $v" }.mkString
      if nested
      then s"\n${indent}let${decls}\n${indent}in ${formatted(body, lvl + 3, false)}"
      else s"let${decls}\n${indent}in ${formatted(body, lvl + 3, false)}"
    case Cond(cond, pass, fail) =>
      val indent = " " * (lvl - 1)
      val scond = formatted(cond, lvl + 1, false)
      val spass = formatted(pass, lvl + 1, false)
      val sfail = formatted(fail, lvl + 1, false)
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
    case rec: RecordVariable =>
      val pairs = rec.fields.map { (k, v) => s"${formatted(k)}: ${formatted(v, label, nested)}" }
      val separator = if nested
                      then ", "
                      else "\n  , "
      s"{ ${pairs.mkString(separator)} }"
    case RecordType(fields) =>
      val pairs = fields.map { (k, v) => s"${formatted(k)}: ${formatted(v, label, nested)}" }
      val separator = if nested
                      then ", "
                      else "\n  , "
      s"{ ${pairs.mkString(separator)} }"
    case LambdaType(tys) =>
      val s = tys.map(formatted(_, label, true)).mkString(" -> ")
      if nested
      then s"($s)"
      else s
    case TypeVariable(id) => label(id)
  }
