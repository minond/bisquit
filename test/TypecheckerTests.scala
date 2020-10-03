package bisquit
package test

import scala.language.implicitConversions

import org.scalatest._
import flatspec._
import matchers._

import nodes._
import parser._
import prelude._
import runtime._
import scope._
import typechecker._

class TypecheckerTests extends AnyFlatSpec with should.Matchers {
  def theTypeOf(code: String): Type =
    parse(code, "<test>").toList.head.right.get match {
      case expr : Expression => return infer(pass1(expr)).right.get
    }

  it should "correctly type value scalars" in {
    IntType() should be(theTypeOf("""1"""))
    IntType() should be(theTypeOf("""4343"""))
    StrType() should be(theTypeOf(""""this is a string""""))
    TupleType(List(IntType(), IntType(), IntType())) should be(theTypeOf("""(1, 2, 3)"""))
  }
}
