package xyz.minond.bisquit

import scala.collection.mutable.ListBuffer

import xyz.minond.bisquit.ast._
import xyz.minond.bisquit.prelude
import xyz.minond.bisquit.runtime.eval
import xyz.minond.bisquit.typechecker._
import xyz.minond.bisquit.scope.typeScope
import xyz.minond.bisquit.printer.formatted

def main(args: Array[String]): Unit =
  val addIt = Func(List(Id("a"), Id("b"), Id("c")),
                   Binop(Id("+"),
                         Binop(Id("+"), Id("a"), Id("b")),
                         Binop(Id("+"), Id("c"), Id("x"))))

  val scope = prelude.Ops ++ Map(
    "addIt" -> addIt,
    "x" -> Int(32),
  )

  var exprs: ListBuffer[Expression] = ListBuffer()

  // exprs += App(Id("addIt"),
  //              List(Binop(Id("+"), Int(32), Int(53)),
  //                   Int(34),
  //                   Int(65)))
  //
  // exprs += App(Func(List(Id("a"), Id("b"), Id("c")),
  //                   Binop(Id("+"),
  //                         Binop(Id("+"), Id("a"), Id("b")),
  //                         Binop(Id("+"), Id("c"), Id("x")))),
  //              List(Binop(Id("+"), Int(32), Int(53))))
  //
  // exprs += App(Func(List(Id("a"), Id("b"), Id("c")),
  //                   Binop(Id("+"),
  //                         Binop(Id("+"), Id("a"), Id("b")),
  //                         Binop(Id("+"), Id("c"), Id("x")))),
  //              List(Binop(Id("+"), Int(32), Int(53)),
  //                   Int(65)))
  //
  // exprs += App(Func(List(Id("a"), Id("b"), Id("c")),
  //                   Binop(Id("+"),
  //                         Binop(Id("+"), Id("a"), Id("b")),
  //                         Binop(Id("+"), Id("c"), Id("x")))),
  //              List(Binop(Id("+"), Int(32), Int(53)),
  //                   Int(34),
  //                   Int(65)))
  //
  // exprs += App(Func(List(Id("a")),
  //                   Uniop(Id("~"), Id("a"))),
  //              List(Int(34)))
  //
  // exprs += App(Func(List(Id("a")),
  //                   Uniop(Id("~"), Id("a"))),
  //              List(Int(34)))
  //
  // exprs += Uniop(Id("~"), Int(43))
  //
  // exprs += Let(Map("a" -> Int(343),
  //                  "b" -> Id("a"),
  //                  "c" -> Binop(Id("+"), Id("a"), Id("b")),
  //                  "x" -> Func(List(Id("x")), Id("x")),
  //                  "d" -> Let(Map("x" -> Id("c")), Id("x"))),
  //              Binop(Id("+"), Id("d"), Id("d")))
  //
  // exprs += Let(Map("x" -> Int(34),
  //                  "y" -> Int(54),
  //                  "z" -> Func(Nil, Binop(Id("+"), Id("x"), Id("y")))),
  //              App(Id("z"), Nil))
  //
  // exprs += Let(Map("a" -> Func(Nil, Binop(Id("+"), Int(2), Int(40))),
  //                  "b" -> Id("a"),
  //                  "c" -> Id("b"),
  //                  "d" -> Id("c")),
  //              App(Id("d"), Nil))
  //
  // exprs += Func(List(), Id("x"))
  //
  // exprs += Bool(true)
  //
  // exprs += Bool(false)
  //
  // exprs += Cons(List(Int(1)))
  //
  // exprs += Cons(List(Bool(true), Bool(false)))
  //
  // exprs += Cons(List(Int(1), Int(2), Int(3)))
  //
  // exprs += Cons(List(
  //   Func(Nil, Binop(Id("+"), Int(2), Int(40))),
  //   Func(Nil, Binop(Id("+"), Int(2), Int(40))),
  //   Func(Nil, Binop(Id("+"), Int(2), Int(40))),
  // ))
  //
  // exprs += Cons(Nil)
  //
  // exprs += Cond(Bool(true),
  //               Int(1),
  //               Int(2))

  exprs += Bool(false)

  exprs += Binop(Id("&&"), Bool(false), Bool(true))

  // exprs += Let(Map("a" -> Int(343),
  //                  "b" -> Func(Nil, Id("a"))),
  //              App(Id("b"), Nil))
  //
  // exprs += App(Let(Map("a" -> Int(30),
  //                      "b" -> Func(List(Id("c")), Binop(Id("+"), Id("a"), Id("c")))),
  //                  Id("b")),
  //              List(Int(34)))

  exprs += Uniop(Id("~"), Int(345))

  exprs += Id("~")

  // exprs += Func(List(Id("a").typeTag(IntType), Id("b").typeTag(IntType), Id("c").typeTag(IntType)),
  //                    Binop(Id("+"),
  //                          Binop(Id("+"), Id("a"), Id("b")),
  //                          Binop(Id("+"), Id("c"), Id("x")))).typeTag(IntType)

  for
    expr <- exprs
  do
    println(s"> ${formatted(expr, 3)}")

    eval(expr, scope) match {
      case Right(ret) => println(s"= ${formatted(ret, 3)}")
      case Left(err) => println(s"error: ${err}")
    }

    deduce(expr, scope) match {
      case Right(ret) => println(s": ${ret}\n")
      case Left(err) => println(s"error: ${err}\n")
    }
