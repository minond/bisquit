package bisquit
package scope

import ast._
import typechecker.Type

type RuntimeScope = Map[String, Value]
type Environment = Map[String, Expression]

object Environment {
  def apply(): Environment = Map()
}
