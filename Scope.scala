package bisquit
package scope

import ast._
import typechecker.Type

type Modules = Map[String, Module]
type Scope = Map[String, Value]
type Environment = Map[Id, Expression]

case class Module(name: String, scope: Scope)
