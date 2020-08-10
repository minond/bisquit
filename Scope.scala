package xyz.minond.bisquit.scope

import xyz.minond.bisquit.ast.Value
import xyz.minond.bisquit.typer.Type

type RuntimeScope = Map[String, Value]
type TypeScope = Map[String, Type]
