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

sealed trait Ty {
  def sub(that: Ty): Boolean =
    (this, that) match {
      case (TyInt, TyInt)   => true
      case (TyReal, TyReal) => true
      case (TyStr, TyStr)   => true
      case (TyBool, TyBool) => true

      // All fields in `this` must exist in `that` and by a subtype as well.
      // Two empty shapes are equal to each other.
      case (TyShape(f1), TyShape(f2)) =>
        f1.foldLeft[Boolean](true) {
          case (eq, (field, ty)) =>
            eq && f2.getOrElse(field, return false).sub(ty)
        }

      case _ => false
    }
}

case object TyInt extends Ty
case object TyReal extends Ty
case object TyStr extends Ty
case object TyBool extends Ty

case class TyShape(fields: Map[String, Ty]) extends Ty

sealed trait TyError

case class UnexpectedTy(expr: Expr, expecting: Ty, got: Ty) extends TyError
case class UnknownTy(name: String) extends TyError

object Ty {
  def of(expr: Expr): Either[TyError, Ty] =
    expr match {
      case Num(_, Real, _, _) => Right(TyReal)
      case _: Num             => Right(TyInt)
      case _: Str             => Right(TyStr)
      case _: Bool            => Right(TyBool)

      case cond: Cond => equationCond(cond)

      case _ => Left(UnknownTy("unimplemented"))
    }

  def equationCond(expr: Cond): Either[TyError, Ty] = {
    val cond = of(expr.cond).fold(err => return Left(err), ok => ok)
    if (!cond.sub(TyBool)) {
      return Left(UnexpectedTy(expr.cond, TyBool, cond))
    }
    Right(TyInt)

    val pass = of(expr.pass).fold(err => return Left(err), ok => ok)
    val fail = of(expr.fail).fold(err => return Left(err), ok => ok)
    if (!pass.sub(fail)) {
      return Left(UnexpectedTy(expr.fail, pass, fail))
    }

    Right(pass)
  }
}
