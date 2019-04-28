package xyz.minond.bisquit.typ

import xyz.minond.bisquit.node

class Typechecker {
  def label(expr: node.Expr): Type =
    label(expr, 1)

  def label(expr: node.Expr, counter: Int): Type =
    expr match {
      case _: node.Num  => Num
      case _: node.Str  => Str
      case _: node.Bool => Bool
      case _            => Poly(s"t$counter")
    }
}
