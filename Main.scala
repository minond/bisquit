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
    "x" -> Num(32),
  )

  var exprs: ListBuffer[Expression] = ListBuffer()

  // exprs += App(Id("addIt"),
  //              List(Binop(Id("+"), Num(32), Num(53)),
  //                   Num(34),
  //                   Num(65)))
  //
  // exprs += App(Func(List(Id("a"), Id("b"), Id("c")),
  //                   Binop(Id("+"),
  //                         Binop(Id("+"), Id("a"), Id("b")),
  //                         Binop(Id("+"), Id("c"), Id("x")))),
  //              List(Binop(Id("+"), Num(32), Num(53))))
  //
  // exprs += App(Func(List(Id("a"), Id("b"), Id("c")),
  //                   Binop(Id("+"),
  //                         Binop(Id("+"), Id("a"), Id("b")),
  //                         Binop(Id("+"), Id("c"), Id("x")))),
  //              List(Binop(Id("+"), Num(32), Num(53)),
  //                   Num(65)))
  //
  // exprs += App(Func(List(Id("a"), Id("b"), Id("c")),
  //                   Binop(Id("+"),
  //                         Binop(Id("+"), Id("a"), Id("b")),
  //                         Binop(Id("+"), Id("c"), Id("x")))),
  //              List(Binop(Id("+"), Num(32), Num(53)),
  //                   Num(34),
  //                   Num(65)))
  //
  // exprs += App(Func(List(Id("a")),
  //                   Uniop(Id("~"), Id("a"))),
  //              List(Num(34)))
  //
  // exprs += App(Func(List(Id("a")),
  //                   Uniop(Id("~"), Id("a"))),
  //              List(Num(34)))
  //
  // exprs += Uniop(Id("~"), Num(43))
  //
  // exprs += Let(Map("a" -> Num(343),
  //                  "b" -> Id("a"),
  //                  "c" -> Binop(Id("+"), Id("a"), Id("b")),
  //                  "x" -> Func(List(Id("x")), Id("x")),
  //                  "d" -> Let(Map("x" -> Id("c")), Id("x"))),
  //              Binop(Id("+"), Id("d"), Id("d")))
  //
  // exprs += Let(Map("x" -> Num(34),
  //                  "y" -> Num(54),
  //                  "z" -> Func(Nil, Binop(Id("+"), Id("x"), Id("y")))),
  //              App(Id("z"), Nil))
  //
  // exprs += Let(Map("a" -> Func(Nil, Binop(Id("+"), Num(2), Num(40))),
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
  // exprs += Cons(List(Num(1)))
  //
  // exprs += Cons(List(Bool(true), Bool(false)))
  //
  // exprs += Cons(List(Num(1), Num(2), Num(3)))
  //
  // exprs += Cons(List(
  //   Func(Nil, Binop(Id("+"), Num(2), Num(40))),
  //   Func(Nil, Binop(Id("+"), Num(2), Num(40))),
  //   Func(Nil, Binop(Id("+"), Num(2), Num(40))),
  // ))
  //
  // exprs += Cons(Nil)
  //
  // exprs += Cond(Bool(true),
  //               Num(1),
  //               Num(2))

  exprs += Bool(false)

  exprs += Binop(Id("&&"), Bool(false), Bool(true))

  // exprs += Let(Map("a" -> Num(343),
  //                  "b" -> Func(Nil, Id("a"))),
  //              App(Id("b"), Nil))
  //
  // exprs += App(Let(Map("a" -> Num(30),
  //                      "b" -> Func(List(Id("c")), Binop(Id("+"), Id("a"), Id("c")))),
  //                  Id("b")),
  //              List(Num(34)))

  exprs += Uniop(Id("~"), Num(345))

  // exprs += Func(List(Id("a").typeTag(NumType), Id("b").typeTag(NumType), Id("c").typeTag(NumType)),
  //                    Binop(Id("+"),
  //                          Binop(Id("+"), Id("a"), Id("b")),
  //                          Binop(Id("+"), Id("c"), Id("x")))).typeTag(NumType)

  for
    expr <- exprs
  do
    println(s"> ${formatted(expr, 3)}")

    eval(expr, scope) match {
      case Right(ret) => println(s"= ${formatted(ret, 3)}")
      case Left(err) => println(s"error: ${err}")
    }

    deduce(expr, typeScope(scope)) match {
      case Right(ret) => println(s": ${ret}\n")
      case Left(err) => println(s"error: ${err}\n")
    }
