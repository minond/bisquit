package bisquit
package printer

import scala.collection.mutable.{Map => MMap}

import errors._
import input._
import nodes.{Int => _, _}
import parser._
import runtime._
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


def formattedAll(exprs: List[Expression], lvl: Int, nested: Boolean, sep: String = " "): String =
  exprs.map(formatted(_, lvl, nested)).mkString(sep)

def formatted(expr: Expression, lvl: Int = 1, nested: Boolean = false, short: Boolean = false): String =
  expr match {
    case id @ Id(lexeme) =>
      id.ty match {
        case None => lexeme
        case Some(ty) => s"${lexeme} : ${formatted(ty)}"
      }
    case Binop(op, left, right) => s"${formatted(left, lvl + 1, false)} ${formatted(op)} ${formatted(right, lvl + 1, false)}"
    case Uniop(op, right) => s"${formatted(op)}${formatted(right, lvl + 1, false)}"
    case App(Id(func), args) => s"${func}(${formattedAll(args, lvl + 1, false, ", ")})"
    case App(fn, args) =>
      val body = formatted(fn, lvl, false)
      val argLvl = body.split("\n").last.size
      val indent = " " * (lvl - 1)
      if nested
      then s"\n${indent}(${body})(${formattedAll(args, argLvl + 3, false, ", ")})"
      else s"(${body})(${formattedAll(args, argLvl + 3, false, ", ")})"
    case Bool(v) => if v then "#t" else "#f"
    case RefCell(value) => s"ref!(${formatted(value, lvl + 1, true, true)})"
    case Lista(Nil) =>
      s"[]"
    case Lista(items) =>
      s"[${formattedAll(items, lvl, true, ", ")}]"
    case Tuple(fields) =>
      s"(${formattedAll(fields, lvl, nested, ", ")})"
    case RecordLookup(rec, field) => s"${formatted(rec, lvl, false)}.${formatted(field, lvl, false)}"
    case Record(fields) =>
      val pairs = fields.map { (k, v) => s"${formatted(k, lvl, false)} = ${formatted(v, lvl, nested, true)}" }
      val indent = " " * (lvl - 1)
      s"{ ${pairs.mkString(s"\n${indent}, ")} }"
    case nodes.Int(num) if num < 0 => s"~${Math.abs(num)}"
    case nodes.Int(num) => num.toString
    case Real(num) if num < 0 => s"~${Math.abs(num)}"
    case Real(num) => num.toString
    case Str(str) => s""""$str""""
    case Lambda(params, body, _) =>
      if short
      then
        "<fn>"
      else
        val indent = " " * lvl
        val spacing = if params.isEmpty then "" else " "
        val args = formattedAll(params, lvl + 1, false, ", ")
        if nested
        then s"\n${indent}fn ($args) = ${formatted(body, lvl + 2, true)}"
        else s"fn ($args) = ${formatted(body, lvl + 2, true)}"
    case _: Builtin => "<builtin>"
    case Let(bindings, body) =>
      val names = bindings.keys
      val values = bindings.values.map { formatted(_, lvl + 2 + 4, false) }
      val argIndent = " " * (lvl + 1)
      val indent = " " * (lvl - 1)
      val decls = names.zip(values).map { (n, v) => s"\n${argIndent}${formatted(n)} = $v" }.mkString
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

def formatted(ty: Type, nested: Boolean): String =
  formatted(ty, Labeler(), nested)

def formatted(ty: Type, label: Labeler, nested: Boolean): String =
  ty match {
    case _: UnitType => "Unit"
    case _: IntType => "Int"
    case _: RealType => "Real"
    case _: OrdType => "Ord"
    case _: NumType => "Num"
    case _: StrType => "Str"
    case _: BoolType => "Bool"
    case ListaType(of) => s"List[${formatted(of, label, true)}]"
    case TupleType(fields) =>
      val tys = fields.map{ field => formatted(field, label, nested) }
      s"(${tys.mkString(" * ")})"
    case rec: RecordVariable =>
      val pairs = rec.fields.map { (k, v) => s"${formatted(k)} : ${formatted(v, label, nested)}" }
      val separator = if nested
                      then ", "
                      else "\n  , "
      s"{ ${pairs.mkString(separator)} }"
    case RecordType(fields) =>
      val pairs = fields.map { (k, v) => s"${formatted(k)} : ${formatted(v, label, nested)}" }
      val separator = if nested
                      then ", "
                      else "\n  , "
      s"{ ${pairs.mkString(separator)} }"
    case LambdaType(tys, vars) =>
      val subbedTys = tys.map {
        case PolymorphicType(_, tyVar) => PolymorphicType(None, tyVar)
        case ty => ty
      }
      val usedPolyVars = tys.foldLeft[List[PolymorphicType]](List.empty) {
        case (acc, ty : PolymorphicType) => acc :+ ty
        case (acc, _) => acc
      }
      val sig = subbedTys.map(formatted(_, label, true)).mkString(" -> ")
      val conds = vars.filter(v => usedPolyVars.contains(v)).map(formatted(_, label, false)).mkString(", ")
      val where =
        if conds.isEmpty
        then ""
        else s" where $conds"
      val whole = s"$sig$where".trim()
      if nested
      then s"($whole)"
      else whole
    case TypeVariable(id) => label(id)
    case PolymorphicType(None, tyVar) =>
      s"${formatted(tyVar, label, false)}"
    case PolymorphicType(Some(parent), tyVar) =>
      if nested
      then s"${formatted(tyVar, label, false)}"
      else s"${formatted(tyVar, label, false)} < ${formatted(parent, label, true)}"
    case RefCellType(of) => s"Ref[${formatted(of, label, true)}]"
  }

def formatted(err: BisquitError, source: String): String =
  err match {
    case UnificationError(ty1, ty2) =>
      ty2.tok match {
        case None =>
          s"given a value of type ${formatted(ty2, true)} where a value of ${formatted(ty1, true)} was expected"
        case Some(tok) =>
          tok.position match {
            case None =>
              s"given a value of type ${formatted(ty2, true)} where a value of ${formatted(ty1, true)} was expected"
            case Some(pos) =>
              getSurroundingLines(pos.offset, source, 5) match {
                case None =>
                  s"given a value of type ${formatted(ty2, true)} where a value of ${formatted(ty1, true)} was expected"
                case Some((lines, lineNumbers, row, col)) =>
                  val buff = StringBuilder()
                  buff.append(s"given a value of type ${formatted(ty2, true)} where a value of ${formatted(ty1, true)} was expected:\n")
                  for (line, num) <- lines.zip(lineNumbers) do
                    val header = s"\n  ${pos.file}:${num + 1} | "
                    if num == row
                    then
                      buff.append(s"$header$line\n")
                      buff.append((" " * (col + header.size - 1)) + "^")
                    else buff.append(s"$header$line")
                  buff.toString
              }
          }
      }

    case _ => err.toString
  }

def errorType(err: BisquitError): String =
  err match {
    case _: TypingError => "type error"
    case _: RuntimeError => "runtime error"
    case _: ParsingError => "parse error"
    case _ => "error"
  }
