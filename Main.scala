package bisquit

import scala.collection.mutable.ListBuffer

import ast._
import prelude.Ops
import runtime._
import typechecker._
import scope.typeScope
import printer.formatted

def main(args: Array[String]): Unit =
  val addIt = Lambda(List(Id("a"), Id("b"), Id("c")),
                     Binop(Id("+"),
                           Binop(Id("+"), Id("a"), Id("b")),
                           Binop(Id("+"), Id("c"), Id("x"))))

  val addTwo = Lambda(List(Id("a"), Id("b")),
                      Binop(Id("+"), Id("a"), Id("b")))

  val identity = Lambda(List(Id("a")),
                        Id("a"))

  val scope = Ops ++ Map(
    "addIt" -> addIt,
    "addTwo" -> addTwo,
    "id" -> identity,
    "x" -> Int(32),
  )

  val subs = Substitution()
  val exprs: ListBuffer[Expression] = ListBuffer()

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

  exprs += Let(Map(Id("a") -> Int(343),
                   Id("b") -> Id("a"),
                   Id("c") -> Binop(Id("+"), Id("a"), Id("b")),
                   Id("d") -> Let(Map(Id("x") -> Id("c")), Id("x"))),
               Binop(Id("+"), Id("d"), Id("d")))

  exprs += Let(Map(Id("a") -> Int(343)),
               Id("a"))

  exprs += Lambda(List(Id("a")),
                  Int(123))

  exprs += Lambda(List(Id("a")),
                  Id("a"))

  exprs += addIt

  exprs += App(Id("+"), List(Int(43), Int(34)))

  exprs += App(Id("~"), List(Int(43)))

  exprs += identity

  exprs += App(identity, List(Int(43)))

  exprs += App(identity, List(Str("FDSA")))

  exprs += App(Id("id"), List(Int(43)))

  exprs += App(Id("id"), List(Str("FDSA")))

  exprs += Lambda(List(Id("a").typeTag(IntType), Id("b").typeTag(IntType), Id("c").typeTag(IntType)),
                     Binop(Id("+"),
                           Binop(Id("+"), Id("a"), Id("b")),
                           Binop(Id("+"), Id("c"), Id("x")))).typeTag(IntType)

  exprs += Lambda(List(Id("a")),
                  Lambda(List(Id("b")),
                         Lambda(List(Id("c")),
                                Binop(Id("+"),
                                      Id("a"),
                                      Binop(Id("+"), Id("b"), Id("c"))))))

  exprs += Lambda(List(Id("a")),
                  Lambda(Nil, Id("a")))

  exprs += App(Lambda(List(Id("a")),
                      Lambda(Nil, Id("a"))), List(Int(42)))

  exprs += App(App(Lambda(List(Id("a")),
                          Lambda(Nil, Id("a"))), List(Int(42))), Nil)

  exprs += App(App(App(App(App(identity, List(identity)), List(identity)), List(identity)), List(identity)), List(Int(42)))

  exprs += App(addIt, List(Str("one"), Str("two"), Str("three")))

  exprs += Lambda(List(Id("a")),
                  Cond(Id("a"),
                       Int(1),
                       Int(0)))

  exprs += Lambda(List(Id("a")),
                  Cond(Id("a"),
                       Id("a"),
                       Id("a")))

  exprs += Lambda(List(Id("a"), Id("b")),
                  Cond(Id("a"),
                       Id("b"),
                       Id("a")))

  exprs += Lambda(List(Id("a"), Id("b")),
                  Cond(Id("a"),
                       Id("+"),
                       Id("b")))

  exprs += Id("+")

  val boolPlusPlus = Lambda(List(Id("a"), Id("b")),
                            Cond(Id("a"),
                                 Id("+"),
                                 Id("b")))
  exprs += App(boolPlusPlus, List(Bool(true), Id("+")))
  exprs += App(boolPlusPlus, List(Bool(false), Id("+")))
  // exprs += App(boolPlusPlus, List(Id("a"), Id("b")))

  exprs += Lambda(List(Id("a"), Id("b"), Id("c")),
                  Cond(Id("b"),
                       Id("a"),
                       Id("c")))

  exprs += Lambda(List(Id("a"), Id("b"), Id("c")),
                  Cond(Id("b"),
                       App(Id("+"), List(Id("b"), Id("c"))),
                       Id("c")))

  exprs += Lambda(List(Id("a")),
                  Lambda(List(Id("a")),
                         Lambda(List(Id("a")),
                                Id("a"))))

  exprs += Lambda(List(Id("a")),
                  Lambda(Nil,
                         Lambda(Nil,
                                Id("a"))))

  exprs += Record(Map(Id("first_name") -> Str("Marcos"),
                      Id("last_name") -> Str("Minond"),
                      Id("age") -> Binop(Id("+"), Int(20), Int(11))))

  val me = Record(Map(Id("first_name") -> Str("Marcos"),
                      Id("last_name") -> Str("Minond"),
                      Id("age") -> Binop(Id("+"), Int(20), Int(11))))
  exprs += RecordLookup(me, Id("first_name"))

  exprs += Lambda(List(Id("a")),
                  RecordLookup(Id("a"), Id("first_name")))

  exprs += Lambda(List(Id("a")),
                  Binop(Id("+"),
                        RecordLookup(Id("a"), Id("age")),
                        Int(34)))

  exprs += Lambda(List(Id("a")),
                  Binop(Id("+"),
                        RecordLookup(Id("a"), Id("age1")),
                        RecordLookup(Id("a"), Id("age2"))))

  exprs += Lambda(List(Id("a")),
                  Let(Map(
                          Id("b") -> Binop(Id("+"),
                                       RecordLookup(Id("a"), Id("age1")),
                                       RecordLookup(Id("a"), Id("age2"))),
                          Id("c") -> Binop(Id("+"),
                                       RecordLookup(Id("a"), Id("age3")),
                                       RecordLookup(Id("a"), Id("age4"))),
                          Id("d") -> Binop(Id("+"), Id("b"), Id("c"))),
                      RecordLookup(Id("a"), Id("age3"))))

  exprs += Lambda(List(Id("a")),
                  Let(Map(
                          Id("b") -> Binop(Id("+"),
                                       RecordLookup(Id("a"), Id("age1")),
                                       RecordLookup(Id("a"), Id("age2"))),
                          Id("c") -> Binop(Id("+"),
                                       RecordLookup(Id("a"), Id("age3")),
                                       RecordLookup(Id("a"), Id("age4"))),
                          Id("d") -> Binop(Id("+"), Id("b"), Id("c"))),
                      Id("a")))

  exprs += Let(Map(Id("a") -> Int(3)), Id("a"))

  exprs += Lambda(List(Id("a"), Id("add")),
                  Let(Map(
                          Id("b") -> Binop(Id("+"),
                                       RecordLookup(Id("a"), Id("age1")),
                                       RecordLookup(Id("a"), Id("age2"))),
                          Id("c") -> Binop(Id("+"),
                                       RecordLookup(Id("a"), Id("age3")),
                                       RecordLookup(Id("a"), Id("age4"))),
                          Id("d") -> Binop(Id("add"), Id("b"), Id("c"))),
                      Id("d")))

  exprs += Lambda(List(Id("a"), Id("add")),
                  Let(Map(
                          Id("b") -> Binop(Id("+"),
                                       RecordLookup(Id("a"), Id("age1")),
                                       RecordLookup(Id("a"), Id("age2"))),
                          Id("c") -> Binop(Id("+"),
                                       RecordLookup(Id("a"), Id("age3")),
                                       RecordLookup(Id("a"), Id("age4"))),
                          Id("d") -> Binop(Id("add"), Id("b"), Id("c")),
                          Id("e") -> Binop(Id("add"), Id("d"), Id("c"))),
                      Id("e")))

  val testfn = Lambda(List(Id("a"), Id("op1"), Id("op2")),
                      Let(Map(
                              Id("b") -> Binop(Id("+"),
                                           RecordLookup(Id("a"), Id("age1")),
                                           RecordLookup(Id("a"), Id("age2"))),
                              Id("c") -> Binop(Id("+"),
                                           RecordLookup(Id("a"), Id("age3")),
                                           RecordLookup(Id("a"), Id("age4"))),
                              Id("d") -> Binop(Id("op1"), Id("b"),
                                           Binop(Id("op2"), Id("c"), Id("c")))),
                          Id("d")))
  val testrec = Record(Map(Id("age1") -> Int(34),
                           Id("age2") -> Int(43),
                           Id("age3") -> Int(43),
                           Id("age4") -> Int(20)))
  exprs += App(testfn, List(testrec, Id("/")))
  exprs += App(testfn, List(testrec, Id("+"), Id("+")))
  // exprs += App(App(testfn, List(testrec, Id("+"))), List(Id("+")))

  exprs += Lambda(List(Id("a")),
                  Let(Map(
                          Id("b") -> Binop(Id("+"),
                                       RecordLookup(Id("a"), Id("age1")),
                                       RecordLookup(Id("a"), Id("age2"))),
                          Id("c") -> Binop(Id("+"),
                                       RecordLookup(Id("a"), Id("age3")),
                                       RecordLookup(Id("a"), Id("age4"))),
                          Id("d") -> Binop(RecordLookup(Id("a"), Id("+")), Id("b"), Id("c")),
                          Id("e") -> Binop(RecordLookup(Id("a"), Id("-")), Id("d"), Id("c"))),
                      Id("e")))

  exprs += Lambda(List(Id("a").typeTag(RecordType(Map(Id("sum") -> LambdaType(List(subs.fresh, subs.fresh, subs.fresh))))), Id("b"), Id("c")),
                  Let(Map(
                          Id("x") -> App(RecordLookup(Id("a"), Id("sum")),
                                     List(Id("b"), Id("c"))),
                          Id("y") -> App(Id("+"),
                                     List(Id("b"), Id("x"))),
                      ),
                      Id("y")))

  exprs += Let(Map(
                   Id("fn1").typeTag(LambdaType(List(IntType, BoolType, IntType))) ->
                     Lambda(List(Id("n"), Id("b")),
                            Cond(Id("b"),
                                 Id("n"),
                                 App(Id("fn2"),
                                     List(App(Id("-"), List(Id("n"), Int(1))),
                                          Bool(true))))),
                   Id("fn2").typeTag(LambdaType(List(IntType, BoolType, IntType))) ->
                     Lambda(List(Id("n"), Id("b")),
                            Cond(Id("b"),
                                 Id("n"),
                                 App(Id("fn1"),
                                     List(App(Id("-"), List(Id("n"), Int(1))),
                                          Bool(true))))),
               ),
               App(Id("fn1"),
                   List(Int(2), Bool(false)))
           )


  for
    expr <- exprs
  do
    println(s"> ${formatted(expr, 3)}")

    eval(pass1(expr), scope) match {
      case Right(ret) => println(s"= ${formatted(ret, 3)}")
      case Left(err) => println(s"error: ${err}")
    }

    infer(pass1(expr), scope, subs) match {
      case Right(ret) => println(s": ${formatted(ret)}\n")
      case Left(err) => println(s"error: ${err}\n")
    }

  // val subs = Substitution()
  // subs.unify(TypeVariable("a"), IntType)
  // subs.unify(TypeVariable("b"), BoolType)
  // subs.unify(TypeVariable("c"), TypeVariable("a"))
  //
  // println(formatted(subs(IntType)))
  // println(formatted(subs(TypeVariable("a"))))
  // println(formatted(subs(TypeVariable("b"))))
  // println(formatted(subs(TypeVariable("c"))))
  // println(formatted(subs(
  //   LambdaType(List(
  //     IntType,
  //     IntType,
  //   ))
  // )))
  // println(formatted(subs(
  //   LambdaType(List(
  //     TypeVariable("a"),
  //     TypeVariable("b"),
  //     TypeVariable("c"),
  //   ))
  // )))
