package xyz.minond.bisquit.runtime

import xyz.minond.bisquit.token._

object Environment {
  type Scope = Map[String, Value]
}

object Evaluator {
  def eval(expr: Expression, scope: Environment.Scope): Value =
    expr match {
      case value: Value => value
      case Id(label) => lookup(label, scope)
      case Binop(op, left, right) => applyOp(op, Some(eval(left, scope)), Some(eval(right, scope)))
      case Uniop(op, subject) => applyOp(op, Some(eval(subject, scope)), None)
      case App(fn, args) => applyFunc(fn, args.map { eval(_, scope) })
    }

  def applyOp(op: Id, left: => Option[Value], right: => Option[Value]): Value =
    (op.lexeme, left, right) match {
      // Binary number operation
      case ("+", Some(l: Num), Some(r: Num)) => applyNumBinop(l, r) { _ + _ }
      case ("-", Some(l: Num), Some(r: Num)) => applyNumBinop(l, r) { _ - _ }
      case ("*", Some(l: Num), Some(r: Num)) => applyNumBinop(l, r) { _ * _ }
      case ("/", Some(l: Num), Some(r: Num)) => applyNumBinop(l, r) { _ / _ }
      case ("%", Some(l: Num), Some(r: Num)) => applyNumBinop(l, r) { _ % _ }

      // Uniary number operations
      case ("-", Some(n: Num), None) => applyNumUniop(n) { -_ }
      case ("+", Some(n: Num), None) => applyNumUniop(n) { Math.abs(_) }
      case ("!", Some(n: Num), None) => applyNumUniop(n) { Math.exp(_) }
    }

  def applyNumBinop(left: Num, right: Num)(f: (Double, Double) => Double): Num =
    Num(f(left.value, right.value))

  def applyNumUniop(right: Num)(f: Double => Double): Num =
    Num(f(right.value))

  def applyFunc(fn: Id, args: => List[Value]): Value =
    ???

  def lookup(label: String, scope: Environment.Scope): Value =
    scope.getOrElse(label, throw Exception(s"lookup error: $label"))
}

