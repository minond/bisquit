package xyz.minond.bisquit

import scala.collection.mutable.ListBuffer

import xyz.minond.bisquit.token._
import xyz.minond.bisquit.runtime.{eval, Scope}
import xyz.minond.bisquit.printer.formatted

def main(args: Array[String]): Unit =
  val addIt = Func(List(Id("a"), Id("b"), Id("c")),
                   Binop(Id("+"),
                         Binop(Id("+"), Id("a"), Id("b")),
                         Binop(Id("+"), Id("c"), Id("x"))))

  val scope = Map(
    "addIt" -> addIt,
    "x" -> Num(32),
    "+" -> numericBinaryBuiltin(_ + _),
    "-" -> numericBinaryBuiltin(_ - _),
    "*" -> numericBinaryBuiltin(_ * _),
    "/" -> numericBinaryBuiltin(_ / _),
    "%" -> numericBinaryBuiltin(_ % _),
    "~" -> numericUnaryBuiltin(-_),
  )

  var exprs: ListBuffer[Expression] = ListBuffer()

  exprs += App(Id("addIt"),
               List(Binop(Id("+"), Num(32), Num(53)),
                    Num(34),
                    Num(65)))

  exprs += App(Func(List(Id("a"), Id("b"), Id("c")),
                    Binop(Id("+"),
                          Binop(Id("+"), Id("a"), Id("b")),
                          Binop(Id("+"), Id("c"), Id("x")))),
               List(Binop(Id("+"), Num(32), Num(53))))

  exprs += App(Func(List(Id("a"), Id("b"), Id("c")),
                    Binop(Id("+"),
                          Binop(Id("+"), Id("a"), Id("b")),
                          Binop(Id("+"), Id("c"), Id("x")))),
               List(Binop(Id("+"), Num(32), Num(53)),
                    Num(65)))

  exprs += App(Func(List(Id("a"), Id("b"), Id("c")),
                    Binop(Id("+"),
                          Binop(Id("+"), Id("a"), Id("b")),
                          Binop(Id("+"), Id("c"), Id("x")))),
               List(Binop(Id("+"), Num(32), Num(53)),
                    Num(34),
                    Num(65)))

  exprs += App(Func(List(Id("a")),
                    Uniop(Id("~"), Id("a"))),
               List(Num(34)))

  exprs += App(Func(List(Id("a")),
                    Uniop(Id("~"), Id("a"))),
               List(Num(34)))

  exprs += Uniop(Id("~"), Num(43))

  exprs += Let(Map("a" -> Num(343),
                   "b" -> Id("a"),
                   "c" -> Binop(Id("+"), Id("a"), Id("b")),
                   "x" -> Func(List(Id("x")), Id("x")),
                   "d" -> Let(Map("x" -> Id("c")), Id("x"))),
               Binop(Id("+"), Id("d"), Id("d")))

  for
    expr <- exprs
  do
    println(s"${formatted(expr)}")
    eval(expr, scope) match {
      case Right(ret) => println(s"===\n${formatted(ret)}\n")
      case Left(err) => println(s"error: ${err}\n")
    }
