package xyz.minond.bisquit

import xyz.minond.bisquit.token._
import xyz.minond.bisquit.runtime.Runtime.eval

object Main {
  def main(args: Array[String]): Unit =
    val addIt = Func(List(Id("a"), Id("b"), Id("c")),
                     Binop(Id("+"),
                           Binop(Id("+"), Id("a"), Id("b")),
                           Binop(Id("+"), Id("c"), Id("x"))))

    val scope = Map("addIt" -> addIt,
                    "x" -> Num(32))

    var expr = App(Id("addIt"),
                   List(Binop(Id("+"), Num(32), Num(53)),
                        Num(34),
                        Num(65)))

    println(eval(expr, scope))
}
