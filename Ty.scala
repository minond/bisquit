package xyz.minond.bisquit

sealed trait Ty {
  def sub(that: Ty): Boolean =
    (this, that) match {
      case (TyInt, TyInt)   => true
      case (TyReal, TyReal) => true
      case (TyStr, TyStr)   => true
      case (TyBool, TyBool) => true

      // All fields in `this` must exist in `that` and be a subtype as well.
      // Two empty shapes are equal to each other.
      case (TyShape(f1), TyShape(f2)) =>
        f1.foldLeft[Boolean](true) {
          case (eq, (field, ty)) =>
            eq && f2.getOrElse(field, return false).sub(ty)
        }

      // All links in `this` must exist in `that` in the same location, and be
      // a subtype as well. Two empty chains are equal to each other.
      case (TyChain(l1), TyChain(l2)) =>
        l1.size <= l2.size && l1.zip(l2).foldLeft[Boolean](true) {
          case (eq, (p1, p2)) => eq && p1.sub(p2)
        }

      case _ => false
    }
}

case object TyInt extends Ty
case object TyReal extends Ty
case object TyStr extends Ty
case object TyBool extends Ty

// TyTy represents a type type
case class TyTy(ty: Ty) extends Ty

// TyChain represents a type definition for a function.
case class TyChain(links: List[Ty]) extends Ty

// TyShape represents a type definition for a record.
case class TyShape(fields: Map[String, Ty]) extends Ty

sealed trait TyError

case class UnexpectedTy(expr: Expr, expecting: Ty, got: Ty) extends TyError
case class UnknownTy(name: String) extends TyError

// XXX Perhaps this should not be a type error
case class UnknownVariableTy(name: String) extends TyError

object Ty {
  def process(
      expr: Expr,
      env: Environment
  ): Either[TyError, (Ty, Environment)] =
    of(expr, env).fold(
      err => Left(err),
      ty => {
        expr match {
          case bind: Binding => Right((ty, env.declare(bind.name, ty)))
          case _             => Right((ty, env))
        }
      }
    )

  def of(expr: Expr, env: Environment): Either[TyError, Ty] =
    expr match {
      case Num(_, Real, _, _) => Right(TyReal)
      case _: Num             => Right(TyInt)
      case _: Str             => Right(TyStr)
      case _: Bool            => Right(TyBool)

      case Identifier(id, _, _) => equationVariableLookup(id, env)
      case cond: Cond           => equationCond(cond, env)
      case _ @Binding(v @ Variable(_, _), b, _) =>
        equationVariableBinding(v, b, env)
    }

  def equationVariableLookup(
      id: String,
      env: Environment
  ): Either[TyError, Ty] =
    env.get(id) match {
      case None     => Left(UnknownVariableTy(id))
      case Some(ty) => Right(ty)
    }

  def equationVariableBinding(
      decl: Variable,
      body: Expr,
      env: Environment
  ): Either[TyError, Ty] =
    decl match {
      case Variable(_, None) => of(body, env)
      case Variable(_, Some(Type(Identifier(ann, _, _)))) =>
        of(body, env) match {
          case Left(err) => Left(err)
          case Right(bty) =>
            env.get(ann) match {
              case Some(TyTy(aty)) =>
                if (bty.sub(aty))
                  Right(aty)
                else
                  Left(UnexpectedTy(body, aty, bty))

              case _ => Left(UnknownTy(ann))
            }
        }
    }

  def equationCond(expr: Cond, env: Environment): Either[TyError, Ty] = {
    val cond = of(expr.cond, env).fold(err => return Left(err), ok => ok)
    if (!cond.sub(TyBool)) {
      return Left(UnexpectedTy(expr.cond, TyBool, cond))
    }

    val pass = of(expr.pass, env).fold(err => return Left(err), ok => ok)
    val fail = of(expr.fail, env).fold(err => return Left(err), ok => ok)
    if (!pass.sub(fail)) {
      return Left(UnexpectedTy(expr.fail, pass, fail))
    }

    Right(pass)
  }
}

case class Environment(uni: Map[String, Ty]) {
  def get(name: String) =
    uni.get(name)

  def declare(name: String, ty: Ty): Environment =
    Environment(uni ++ Map(name -> ty))
}

object Environment {
  def create(): Environment =
    Environment(
      Map(
        "int" -> TyTy(TyInt),
        "real" -> TyTy(TyReal),
        "string" -> TyTy(TyStr),
        "bool" -> TyTy(TyBool)
      )
    )
}
