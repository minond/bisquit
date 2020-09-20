package bisquit
package scope

import ast._
import typechecker.Type

type Modules = Map[String, Module]
type Scope = Map[Id, Value]
type Environment = Map[Id, Expression]

case class Module(name: String, scope: Scope)
