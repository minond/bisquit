package xyz.minond.bisquit.typ

import scala.collection.mutable.Map

import xyz.minond.bisquit.node

class Typechecker {
  val registry = Map[String, Type]()

  def register(name: String, typ: Type) =
    registry += (name -> typ)

  register("bool", Bool)
  register("number", Num)
  register("string", Str)

  def infer(expr: node.Expr): Type =
    expr match {
      case _: node.Num  => Num
      case _: node.Str  => Str
      case _: node.Bool => Bool

      case node.Cond(cond, pass, fail, _) =>
        expect(Bool, cond)(return _)
        expect(infer(pass), fail)(return _)

      case node.Binding(node.Variable(_, typ), body, _) =>
        typ match {
          case Some(typ) => expect(annotation(typ), body)(return _)
          case None      => infer(body)
        }
    }

  def expect(expecting: Type, expr: node.Expr)(
      handler: Error => Type
  ): Type =
    infer(expr) match {
      case got if got != expecting => handler(Unexpected(expr, expecting, got))
      case got                     => got
    }

  def annotation(typ: node.Type): Type =
    typ match {
      case node.Type(node.Identifier(name, _, _)) => annotation(name)
    }

  def annotation(name: String): Type =
    registry.get(name) match {
      case Some(name) => name
      case None       => Unknown(name)
    }
}
