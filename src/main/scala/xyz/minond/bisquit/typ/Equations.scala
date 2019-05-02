package xyz.minond.bisquit.typ

import xyz.minond.bisquit.node.{Cond, Expr}

sealed trait Ty

case class TyVar(label: String) extends Ty
case object TyNum extends Ty
case object TyStr extends Ty
case object TyBool extends Ty

sealed trait Error extends Ty

case class Unexpected(expr: Expr, expecting: Ty, got: Ty) extends Error
case class Unknown(name: String) extends Error

sealed trait TyEq {
  def exprs(env: Environment): List[Ty]
}

case class CondTyEq(cond: Cond) extends TyEq {
  def exprs(env: Environment) = {
    val tycond = Labeler.label(env, cond.cond)
    val typass = Labeler.label(env, cond.pass)
    val tyfail = Labeler.label(env, cond.fail)
    List(typass, tycond, typass, tyfail)
  }
}
