package xyz.minond.bisquit

import xyz.minond.bisquit.token.{Id, Num, Binop, Uniop}
import xyz.minond.bisquit.input.Positioned
import xyz.minond.bisquit.runtime.{Evaluator, Environment}

object Main {
  def main(args: Array[String]): Unit =
    val s1 = Binop(Id("-"),
                   Id("a"),
                   Id("b"))

    val scope = Map("a" -> Num(22),
                    "b" -> Num(20))

    println(s1)
    println(Evaluator.eval(s1, scope))
    println(Evaluator.eval(Uniop(Id("!"), Id("a")), scope))
    println(Evaluator.eval(Uniop(Id("-"), Id("a")), scope))
    println(Positioned.stdin(Id("hi"), 0, 0).position.get.toString)
}
