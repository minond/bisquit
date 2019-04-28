package xyz.minond.bisquit.typ

import xyz.minond.bisquit.node.Expr

sealed trait Type

case class Poly(label: String) extends Type
case object Num extends Type
case object Str extends Type
case object Bool extends Type

sealed trait Error extends Type

case class Unexpected(expr: Expr, expecting: Type, got: Type) extends Error
case class Unknown(name: String) extends Error
