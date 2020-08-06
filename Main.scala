package xyz.minond.bisquit

import xyz.minond.bisquit.token._
import xyz.minond.bisquit.runtime.Runtime.eval
import xyz.minond.bisquit.printer.formatted

object Main {
  def main(args: Array[String]): Unit =
    val addIt = Func(List(Id("a"), Id("b"), Id("c")),
                     Binop(Id("+"),
                           Binop(Id("+"), Id("a"), Id("b")),
                           Binop(Id("+"), Id("c"), Id("x"))))

    val scope = Map("addIt" -> addIt,
                    "x" -> Num(32))

    var app1 = App(Id("addIt"),
                   List(Binop(Id("+"), Num(32), Num(53)),
                        Num(34),
                        Num(65)))

    var app2 = App(Func(List(Id("a"), Id("b"), Id("c")),
                        Binop(Id("+"),
                              Binop(Id("+"), Id("a"), Id("b")),
                              Binop(Id("+"), Id("c"), Id("x")))),
                   List(Binop(Id("+"), Num(32), Num(53)),
                        Num(34),
                        Num(65)))

    println(eval(app1, scope))
    println(eval(app2, scope))
    println(formatted(addIt))
    println(formatted(app1))
    println(formatted(app2))
}
