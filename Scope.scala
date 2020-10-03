package bisquit
package scope

import nodes._
import typechecker.Type


type Modules = Map[Id, Module]
type Scope = Map[Id, Value]
type Environment = Map[Id, Expression]
