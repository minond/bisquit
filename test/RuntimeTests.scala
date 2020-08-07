package xyz.minond.bisquit.test

import scala.language.implicitConversions

import org.scalatest._
import flatspec._
import matchers._

import xyz.minond.bisquit.ast._
import xyz.minond.bisquit.prelude
import xyz.minond.bisquit.runtime.{eval, Scope}

class RuntimeTests extends AnyFlatSpec with should.Matchers {
  it should "evaluate scalars" in {
    eval(Num(3)) should be (Right(Num(3)))
    eval(Num(-3)) should be (Right(Num(-3)))
    eval(Bool(true)) should be (Right(Bool(true)))
    eval(Bool(false)) should be (Right(Bool(false)))
  }

  it should "add two numbers" in {
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
    val expr = App(App(App(App(Let(Map("a" -> Num(222),
                                       "b" -> Num(999999),
                                       "x" -> Num(999999)),
                                   Let(Map("b" -> Num(343),
                                           "x" -> Num(999999)),
                                       Func(List(Id("x")),
                                            Binop(Id("+"),
                                                  Id("a"),
                                                  Binop(Id("+"),
                                                        Id("b"),
                                                        Id("x")))))),
                               Nil),
                           Nil),
                       Nil),
                   List(Num(43)))

    eval(expr, prelude.Ops) should be (Right(Num(608)))
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
    val expr = App(App(App(App(Let(Map("a" -> Num(222),
                                       "b" -> Num(999999),
                                       "x" -> Num(999999)),
                                   Let(Map("b" -> Num(343),
                                           "x" -> Num(999999)),
                                       Func(List(Id("x")),
                                            Binop(Id("+"),
                                                  Id("a"),
                                                  Binop(Id("+"),
                                                        Id("b"),
                                                        Id("x")))))),
                               Nil),
                           Nil),
                       Nil),
                   List(Num(43)))

    eval(expr, prelude.Ops) should be (Right(Num(608)))
  }
}
