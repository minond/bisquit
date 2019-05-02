package xyz.minond.bisquit.typ

import xyz.minond.bisquit.node.{Cond, Expr}

sealed trait Ty {
  def equals(other: Ty): Boolean =
    (this, other) match {
      case (TyNum, TyNum)                          => true
      case (TyStr, TyStr)                          => true
      case (TyBool, TyBool)                        => true
      case (TyVar(l1), TyVar(l2)) if l1.equals(l2) => true
      case _                                       => false
    }
}

case class TyVar(label: String) extends Ty
case object TyNum extends Ty
case object TyStr extends Ty
case object TyBool extends Ty

sealed trait Error extends Ty

case class Unexpected(expr: Expr, expecting: Ty, got: Ty) extends Error
case class Unknown(name: String) extends Error

sealed trait TyEq {
  def exprs(env: Environment): Either[Error, List[Ty]]
}

case class CondTyEq(expr: Cond) extends TyEq {
  def exprs(env: Environment): Either[Error, List[Ty]] = {
    val cond = Labeler.label(env, expr.cond)
    if (!cond.equals(TyBool)) {
      return Left(Unexpected(expr.cond, TyBool, cond))
    }

    val pass = Labeler.label(env, expr.pass)
    val fail = Labeler.label(env, expr.fail)
    if (!cond.equals(TyBool)) {
      return Left(Unexpected(expr.fail, pass, fail))
    }

    Right(List(pass, cond, pass, fail))
  }
}
