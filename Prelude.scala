package bisquit
package prelude

import errors._
import nodes._
import printer._
import runtime._
import scope._
import typechecker._
import utils.ensure


val tyVar1 = fresh()
val polyNum = PolymorphicType(Some(NumType()))
val polyOrd1 = PolymorphicType(Some(OrdType()))
val polyOrd2 = PolymorphicType(Some(OrdType()))

def signature(tys: List[Type], vars: List[PolymorphicType] = List.empty) =
  LambdaType(tys, vars)

// TODO Add ArgumentTypeError back:
// num <- ensure[RuntimeError, Int](value, ArgumentTypeError(expr))
def numericBinaryBuiltin(f: (Double, Double) => Double): Builtin =
  Builtin(signature(List(polyNum, polyNum, polyNum), List(polyNum)), {
    case (l :: r :: Nil, scope) =>
      for
        leftVal <- eval(l, scope)
        rightVal <- eval(r, scope)
      yield
        (leftVal, rightVal) match {
          case (Int(left), Int(right)) => Int(f(left.toDouble, right.toDouble).toInt)
          case (Real(left), Real(right)) => Real(f(left, right))
          case (Int(left), Real(right)) => Real(f(left.toDouble, right))
          case (Real(left), Int(right)) => Real(f(left, right.toDouble))
        }
  })

// TODO Add ArgumentTypeError back:
// num <- ensure[RuntimeError, Int](value, ArgumentTypeError(expr))
def numericUnaryBuiltin(f: Double => Double): Builtin =
  Builtin(signature(List(polyNum, polyNum), List(polyNum)), {
    case (expr :: Nil, scope) =>
      for
        value <- eval(expr, scope)
      yield
        value match {
          case Int(num) => Int(f(num.toDouble).toInt)
          case Real(num) => Real(f(num))
        }
  })

val booleanAnd = Builtin(signature(List(BoolType(), BoolType(), BoolType())), {
  case (left :: right :: Nil, scope) =>
    eval(left, scope).flatMap {
      case Bool(true) => eval(right, scope)
      case Bool(false) => Right(Bool(false))
      case invalid => Left(ArgumentTypeError(pass1(invalid)))
    }
})

val booleanOr = Builtin(signature(List(BoolType(), BoolType(), BoolType())), {
  case (left :: right :: Nil, scope) =>
    eval(left, scope).flatMap {
      case Bool(true) => Right(Bool(true))
      case Bool(false) => eval(right, scope)
    }
})

val InternalModuleName = Id("Internal")
val InternalFunctions = Map(
  Id("~sum-bin-num") -> numericBinaryBuiltin(_ + _),
  Id("~sub-bin-num") -> numericBinaryBuiltin(_ - _),
  Id("~mul-bin-num") -> numericBinaryBuiltin(_ * _),
  Id("~div-bin-num") -> numericBinaryBuiltin(_ / _),
  Id("~mod-bin-num") -> numericBinaryBuiltin(_ % _),
  Id("~neg-uni-num") -> numericUnaryBuiltin(-_),
  Id("~or-bin-bool") -> booleanOr,
  Id("~and-bin-bool") -> booleanAnd,

  Id("~eq-bin-ord") ->
    Builtin(signature(List(polyOrd1, polyOrd2, BoolType()), List(polyOrd1, polyOrd2)), {
      case (l :: r :: Nil, scope) =>
        for
          left <- eval(l, scope)
          right <- eval(r, scope)
        yield
          Bool(left == right)
    }),

  Id("~ref-cell-ref!") ->
    Builtin(signature(List(tyVar1, RefCellType(tyVar1))), {
      case (value :: Nil, scope) =>
        for
          evaled <- eval(value, scope)
        yield
          RefCell(evaled)
    }),

  Id("~ref-cell-get!") ->
    Builtin(signature(List(RefCellType(tyVar1), tyVar1)), {
      case (ref :: Nil, scope) =>
        for
          evaled <- eval(ref, scope)
          refVal <- ensure[RuntimeError, RefCell](evaled, ArgumentTypeError(ref))
        yield
          refVal.value
    }),

  Id("~ref-cell-set!") ->
    Builtin(signature(List(RefCellType(tyVar1), tyVar1, RefCellType(tyVar1))), {
      case (ref :: value :: Nil, scope) =>
        for
          maybeRefVal <- eval(ref, scope)
          refVal <- ensure[RuntimeError, RefCell](maybeRefVal, ArgumentTypeError(ref))
          evaled <- eval(value, scope)
        yield
          refVal.value = evaled
          RefCell(evaled)
    }),

  Id("~list-cons") ->
    Builtin(signature(List(tyVar1, ListaType(tyVar1), ListaType(tyVar1))), {
      case (headExpr :: tailExpr :: Nil, scope) =>
        for
          head <- eval(headExpr, scope)
          tailMaybe <- eval(tailExpr, scope)
          tail <- ensure[RuntimeError, Lista](tailMaybe, ArgumentTypeError(tailExpr))
        yield
          Lista(head +: tail.items)
    }),

  Id("~list-car") ->
    Builtin(signature(List(ListaType(tyVar1), tyVar1)), {
      case (listExpr :: Nil, scope) =>
        for
          listMaybe <- eval(listExpr, scope)
          list <- ensure[RuntimeError, Lista](listMaybe, ArgumentTypeError(listExpr))
          headExpr <- if list.items.isEmpty
                      then Left(CannotGetCarOfEmptyList(listExpr))
                      else Right(list.items.head)
          head <- eval(pass1(headExpr), scope)
        yield
          head
    }),

  Id("~list-cdr") ->
    Builtin(signature(List(ListaType(tyVar1), ListaType(tyVar1))), {
      case (listExpr :: Nil, scope) =>
        for
          listMaybe <- eval(listExpr, scope)
          list <- ensure[RuntimeError, Lista](listMaybe, ArgumentTypeError(listExpr))
          tailExpr = if list.items.isEmpty
                     then List.empty
                     else list.items.tail
          tail <- eval(tailExpr.map(pass1), scope)
        yield
          Lista(tail)
    }),

  Id("~list-nil") ->
    Builtin(signature(List(ListaType(tyVar1), BoolType())), {
      case (listExpr :: Nil, scope) =>
        for
          listMaybe <- eval(listExpr, scope)
          list <- ensure[RuntimeError, Lista](listMaybe, ArgumentTypeError(listExpr))
        yield
          Bool(list.items.isEmpty)
    }),

  Id("int_to_real") ->
    Builtin(signature(List(IntType(), RealType())), {
      case (arg :: Nil, scope) =>
        eval(arg, scope).flatMap {
          case Int(int) => Right(Real(int.toDouble))
        }
    }),

  Id("universe") ->
    Builtin(signature(List(UnitType(), UnitType())), { (_, scope) =>
      scope.map { case ((k, v)) =>
        println(s"${k.lexeme} : ${infer(pass1(v), scope, Substitution()).map(ty => formatted(ty)).right.get}")
      }
      Right(Tuple(List.empty))
    }),
)

val Prelude: Modules = Map(
  InternalModuleName -> Module(InternalModuleName,
                               InternalFunctions.keys.toSet,
                               InternalFunctions),
)
