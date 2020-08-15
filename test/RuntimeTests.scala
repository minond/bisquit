package xyz.minond.bisquit.test

import scala.language.implicitConversions

import org.scalatest._
import flatspec._
import matchers._

import xyz.minond.bisquit.ast._
import xyz.minond.bisquit.scope._
import xyz.minond.bisquit.prelude
import xyz.minond.bisquit.runtime.eval

class RuntimeTests extends AnyFlatSpec with should.Matchers {
  it should "evaluate scalars" in {
    eval(Int(3)) should be (Right(Int(3)))
    eval(Int(-3)) should be (Right(Int(-3)))
    eval(Bool(true)) should be (Right(Bool(true)))
    eval(Bool(false)) should be (Right(Bool(false)))
  }

  it should "add two numbers" in {
    // (let
    //    a = 343.0
    //    b = 54.0
    //  in \->
    //       a + b)()
    val expr = App(Let(Map("a" -> Int(343),
                           "b" -> Int(54)),
                       Lambda(Nil, Binop(Id("+"), Id("a"), Id("b")))),
                   Nil)

    eval(expr, prelude.Ops) should be (Right(Int(397)))
  }

  it should "use lexically scope bindings" in {
    // ((((let
    //       a = 222.0
    //       b = 999999.0
    //       x = 999999.0
    //     in let
    //          b = 343.0
    //          x = 999999.0
    //        in \x ->
    //             a + b + x)())())())(43.0)
    //
    // # => 608 : num
    val expr = App(App(App(App(Let(Map("a" -> Int(222),
                                       "b" -> Int(999999),
                                       "x" -> Int(999999)),
                                   Let(Map("b" -> Int(343),
                                           "x" -> Int(999999)),
                                       Lambda(List(Id("x")),
                                              Binop(Id("+"),
                                                    Id("a"),
                                                    Binop(Id("+"),
                                                          Id("b"),
                                                          Id("x")))))),
                               Nil),
                           Nil),
                       Nil),
                   List(Int(43)))

    eval(expr, prelude.Ops) should be (Right(Int(608)))
  }

  it should "branch to the then body" in {
    // let
    //   id = \x -> x
    // in if id(#f)
    //    then 1.0
    //    else 2.0
    val expr = Let(Map("id" -> Lambda(List(Id("x")), Id("x"))),
                   Cond(App(Id("id"), List(Bool(true))),
                        Int(1),
                        Int(2)))

    eval(expr, prelude.Ops) should be (Right(Int(1)))
  }

  it should "branch to the else body" in {
    // let
    //   id = \x -> x
    // in if id(#t)
    //    then 1.0
    //    else 2.0
    val expr = Let(Map("id" -> Lambda(List(Id("x")), Id("x"))),
                   Cond(App(Id("id"), List(Bool(false))),
                        Int(1),
                        Int(2)))

    eval(expr, prelude.Ops) should be (Right(Int(2)))
  }
}
