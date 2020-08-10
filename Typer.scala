package xyz.minond.bisquit.typer

import xyz.minond.bisquit.ast._
import xyz.minond.bisquit.scope._
import xyz.minond.bisquit.runtime._

sealed trait Type
case object UnitType extends Type
case object NumType extends Type
case object BoolType extends Type
case class FuncType(tys: Type*) extends Type

type Signature = FuncType
