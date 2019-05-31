package xyz.minond.bisquit

// case class Environment(uni: Map[String, Ty]) {
//   def get(name: String) =
//     uni.get(name)
// }
//
// object Environment {
//   def create(): Environment =
//     Environment(Map())
// }

// sealed trait Ty {
//   def equals(other: Ty): Boolean =
//     (this, other) match {
//       case (TyNum, TyNum)                          => true
//       case (TyStr, TyStr)                          => true
//       case (TyBool, TyBool)                        => true
//       case (TyVar(l1), TyVar(l2)) if l1.equals(l2) => true
//       case _                                       => false
//     }
// }

// case class TyVar(label: String) extends Ty
// case object TyNum extends Ty
// case object TyStr extends Ty
// case object TyBool extends Ty
//
// sealed trait TyError extends Ty
//
// case class UnexpectedTy(expr: Expr, expecting: Ty, got: Ty) extends TyError
// case class UnknownTy(name: String) extends TyError
//
// sealed trait TyEq {
//   def exprs(env: Environment): Either[TyError, List[Ty]]
// }
//
// case class CondTyEq(expr: Cond) extends TyEq {
//   def exprs(env: Environment): Either[TyError, List[Ty]] = {
//     val cond = Labeler.label(env, expr.cond)
//     if (!cond.equals(TyBool)) {
//       return Left(UnexpectedTy(expr.cond, TyBool, cond))
//     }
//
//     val pass = Labeler.label(env, expr.pass)
//     val fail = Labeler.label(env, expr.fail)
//     if (!cond.equals(TyBool)) {
//       return Left(UnexpectedTy(expr.fail, pass, fail))
//     }
//
//     Right(List(pass, cond, pass, fail))
//   }
// }
//
// object Labeler {
//   def label(env: Environment, expr: Expr): Ty =
//     label(env, expr, 1)
//
//   def label(env: Environment, expr: Expr, counter: Int): Ty =
//     expr match {
//       case _: Num  => TyNum
//       case _: Str  => TyStr
//       case _: Bool => TyBool
//       case _       => TyVar(s"t$counter")
//     }
// }
