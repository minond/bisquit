package xyz.minond.bisquit.typ

import xyz.minond.bisquit.node

object Labeler {
  def label(env: Environment, expr: node.Expr): Ty =
    label(env, expr, 1)

  def label(env: Environment, expr: node.Expr, counter: Int): Ty =
    expr match {
      case _: node.Num  => TyNum
      case _: node.Str  => TyStr
      case _: node.Bool => TyBool
      case _            => TyVar(s"t$counter")
    }
}
