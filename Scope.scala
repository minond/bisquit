package xyz.minond.bisquit.scope

import xyz.minond.bisquit.ast.{Builtin, Value}
import xyz.minond.bisquit.typechecker.Type

type RuntimeScope = Map[String, Value]
type TypeScope = Map[String, Type]

def typeScope(scope: RuntimeScope): TypeScope =
  scope.foldLeft[TypeScope](Map()) {
    case (acc, (label, value)) =>
      value match {
        case Builtin(signature, _) => acc ++ Map(label -> signature)
        case _ => acc
      }
  }
