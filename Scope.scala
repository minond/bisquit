package bisquit
package scope

import ast.{Builtin, Expression, Value}
import typechecker.Type

type RuntimeScope = Map[String, Value]
type TypeScope = Map[String, Type]
type Environment = Map[String, Expression]

def typeScope(scope: RuntimeScope): TypeScope =
  scope.foldLeft[TypeScope](Map()) {
    case (acc, (label, value)) =>
      value match {
        case Builtin(signature, _) => acc ++ Map(label -> signature)
        case _ => acc
      }
  }
