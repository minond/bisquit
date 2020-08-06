package xyz.minond.bisquit

import scala.collection.mutable.ListBuffer

import xyz.minond.bisquit.token._
import xyz.minond.bisquit.runtime.{eval, Scope}
import xyz.minond.bisquit.printer.formatted

def evalAndPrintIt(expr: Expression, scope: Scope) =
  println(s"> ${formatted(expr)}")
  eval(expr, scope).map { x => println(s"= ${formatted(x)}") }

def main(args: Array[String]): Unit =
  val addIt = Func(List(Id("a"), Id("b"), Id("c")),
                   Binop(Id("+"),
                         Binop(Id("+"), Id("a"), Id("b")),
                         Binop(Id("+"), Id("c"), Id("x"))))

  val scope = Map("addIt" -> addIt,
                  "x" -> Num(32))

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

  for
    expr <- exprs
  do
    println(s"> ${formatted(expr)}")
    eval(expr, scope) match {
      case Right(ret) => println(s"= ${formatted(ret)}\n")
      case Left(err) => println(s"error: ${err}\n")
    }
