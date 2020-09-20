package bisquit
package scope

import ast._
import typechecker.Type

type Modules = Map[Id, Module]
type Scope = Map[Id, Value]
type Environment = Map[Id, Expression]

case class Module(name: Id, scope: Scope)
