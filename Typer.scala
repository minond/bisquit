package xyz.minond.bisquit.typer

import xyz.minond.bisquit.ast._
import xyz.minond.bisquit.runtime.{Scope => _, _}

sealed trait Ty
case object UnitTy extends Ty
case object NumTy extends Ty
case object BoolTy extends Ty
case class FuncTy(tys: Ty*) extends Ty

type Signature = FuncTy
