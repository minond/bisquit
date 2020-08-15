package bisquit
package scope

import ast.{Builtin, Value}
import typechecker.Type

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
