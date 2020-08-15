package bisquit

import scala.collection.mutable.ListBuffer

import ast._
import prelude.Ops
import runtime.eval
import typechecker._
import scope.typeScope
import printer.formatted

def main(args: Array[String]): Unit =
  val addIt = Lambda(List(Id("a"), Id("b"), Id("c")),
                     Binop(Id("+"),
                           Binop(Id("+"), Id("a"), Id("b")),
                           Binop(Id("+"), Id("c"), Id("x"))))

  val scope = Ops ++ Map(
    "addIt" -> addIt,
    "x" -> Int(32),
  )

  var exprs: ListBuffer[Expression] = ListBuffer()

  // exprs += App(Id("addIt"),
  //              List(Binop(Id("+"), Int(32), Int(53)),
  //                   Int(34),
  //                   Int(65)))
  //
  // exprs += App(Lambda(List(Id("a"), Id("b"), Id("c")),
  //                   Binop(Id("+"),
  //                         Binop(Id("+"), Id("a"), Id("b")),
  //                         Binop(Id("+"), Id("c"), Id("x")))),
  //              List(Binop(Id("+"), Int(32), Int(53))))
  //
  // exprs += App(Lambda(List(Id("a"), Id("b"), Id("c")),
  //                   Binop(Id("+"),
  //                         Binop(Id("+"), Id("a"), Id("b")),
  //                         Binop(Id("+"), Id("c"), Id("x")))),
  //              List(Binop(Id("+"), Int(32), Int(53)),
  //                   Int(65)))
  //
  // exprs += App(Lambda(List(Id("a"), Id("b"), Id("c")),
  //                   Binop(Id("+"),
  //                         Binop(Id("+"), Id("a"), Id("b")),
  //                         Binop(Id("+"), Id("c"), Id("x")))),
  //              List(Binop(Id("+"), Int(32), Int(53)),
  //                   Int(34),
  //                   Int(65)))
  //
  // exprs += App(Lambda(List(Id("a")),
  //                   Uniop(Id("~"), Id("a"))),
  //              List(Int(34)))
  //
  // exprs += App(Lambda(List(Id("a")),
  //                   Uniop(Id("~"), Id("a"))),
  //              List(Int(34)))
  //
  // exprs += Uniop(Id("~"), Int(43))
  //
  // exprs += Let(Map("a" -> Int(343),
  //                  "b" -> Id("a"),
  //                  "c" -> Binop(Id("+"), Id("a"), Id("b")),
  //                  "x" -> Lambda(List(Id("x")), Id("x")),
  //                  "d" -> Let(Map("x" -> Id("c")), Id("x"))),
  //              Binop(Id("+"), Id("d"), Id("d")))
  //
  // exprs += Let(Map("x" -> Int(34),
  //                  "y" -> Int(54),
  //                  "z" -> Lambda(Nil, Binop(Id("+"), Id("x"), Id("y")))),
  //              App(Id("z"), Nil))
  //
  // exprs += Let(Map("a" -> Lambda(Nil, Binop(Id("+"), Int(2), Int(40))),
  //                  "b" -> Id("a"),
  //                  "c" -> Id("b"),
  //                  "d" -> Id("c")),
  //              App(Id("d"), Nil))
  //
  // exprs += Lambda(List(), Id("x"))
  //
  // exprs += Bool(true)
  //
  // exprs += Bool(false)
  //
  // exprs += Cond(Bool(true),
  //               Int(1),
  //               Int(2))

  exprs += Bool(false)

  exprs += Binop(Id("&&"), Bool(false), Bool(true))

  // exprs += Let(Map("a" -> Int(343),
  //                  "b" -> Lambda(Nil, Id("a"))),
  //              App(Id("b"), Nil))
  //
  // exprs += App(Let(Map("a" -> Int(30),
  //                      "b" -> Lambda(List(Id("c")), Binop(Id("+"), Id("a"), Id("c")))),
  //                  Id("b")),
  //              List(Int(34)))

  exprs += Uniop(Id("~"), Int(345))

  exprs += Str("~")

  exprs += Id("~")

  exprs += Let(Map("a" -> Int(343),
                   "b" -> Id("a"),
                   "c" -> Binop(Id("+"), Id("a"), Id("b")),
                   "d" -> Let(Map("x" -> Id("c")), Id("x"))),
               Binop(Id("+"), Id("d"), Id("d")))

  exprs += Let(Map("a" -> Int(343)),
               Id("a"))

  // exprs += Lambda(List(Id("a").typeTag(IntType), Id("b").typeTag(IntType), Id("c").typeTag(IntType)),
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
      case Right(ret) => println(s": ${formatted(ret)}\n")
      case Left(err) => println(s"error: ${err}\n")
    }
