package xyz.minond.bisquit

sealed trait Ty {
  def sub(that: Ty): Boolean =
    (this, that) match {
      case (TyBool, TyBool) => true
      case (TyInt, TyInt)   => true
      case (TyReal, TyReal) => true
      case (TyStr, TyStr)   => true
      case (TyUnit, TyUnit) => true

      // All fields in `this` must exist in `that` and be a subtype as well.
      // Two empty shapes are equal to each other.
      case (TyShape(f1), TyShape(f2)) =>
        f1.foldLeft[Boolean](true) {
          case (eq, (field, ty)) =>
            eq && f2.getOrElse(field, return false).sub(ty)
        }

      // All fields in `this` must exist in `that` in the same location, and be
      // a subtype as well. Two empty tuples are units and therefore equal to
      // each other.
      case (TyTuple(f1), TyTuple(f2)) =>
        f1.size == f2.size && f1.zip(f2).foldLeft[Boolean](true) {
          case (eq, (p1, p2)) => eq && p1.sub(p2)
        }

      case _ => false
    }

  override def toString() =
    this match {
      case TyBool => Ty.NameTyBool
      case TyInt  => Ty.NameTyInt
      case TyReal => Ty.NameTyReal
      case TyStr  => Ty.NameTyStr
      case TyUnit => Ty.NameTyUnit
      case _      => this.toStringPretty(0)
    }

  def toStringPretty(level: Int): String =
    this match {
      case TyTy(ty) => s"type<${ty.toStringPretty(level + 2)}>"
      case TyLambda(links) =>
        s"func : ${links.map(_.toStringPretty(level + 2)).mkString(" -> ")}"
      case TyTuple(Nil) =>
        TyUnit.toString
      case TyTuple(fields) =>
        s"(${fields.map(_.toStringPretty(level + 2)).mkString(" * ")})"
      case TyShape(_) => "record"
      case _          => this.toString
    }
}

case object TyBool extends Ty
case object TyInt extends Ty
case object TyReal extends Ty
case object TyStr extends Ty
case object TyUnit extends Ty
case class TyTuple(fields: List[Ty]) extends Ty
case class TyTy(ty: Ty) extends Ty
case class TyShape(fields: Map[String, Ty]) extends Ty
case class TyLambda(links: List[Ty]) extends Ty

sealed trait TyError {
  override def toString() =
    this match {
      case UnknownTy(name) =>
        s"type error: unknown type `$name`"
      case UnknownTyOf(name) =>
        s"type error: unknown type of `$name`"
      case UnexpectedTy(_, expecting, got) =>
        s"type error: expecting $expecting but got $got instead"
      case UnknownVariableTy(name) =>
        s"type error: unable to lookup type for `$name`"
    }
}

case class UnexpectedTy(expr: Expr, expecting: Ty, got: Ty) extends TyError
case class UnknownTy(name: String) extends TyError
case class UnknownTyOf(name: String) extends TyError

// XXX Perhaps this should not be a type error
case class UnknownVariableTy(name: String) extends TyError

object Ty {
  val NameTyBool = "bool"
  val NameTyInt = "int"
  val NameTyReal = "real"
  val NameTyStr = "string"
  val NameTyUnit = "unit"

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

      case Identifier(id, _, _)          => ruleLookup(id, env)
      case cond: Cond                    => ruleCond(cond, env)
      case Let(bindings, body, _)        => ruleLet(bindings, body, env)
      case app @ App(fn, args, _)        => ruleApp(app, fn.lexeme, args, env)
      case _ @Binding(v: Variable, b, _) => ruleVar(v, b, env)
      case _ @Binding(f: Function, b, _) => ruleFunc(f, b, env)
    }

  /** x : a ∈ Γ
    * ---------
    * Γ ⊢ x : a
    */
  def ruleLookup(
      id: String,
      env: Environment
  ): Either[TyError, Ty] =
    env.get(id) match {
      case None     => Left(UnknownVariableTy(id))
      case Some(ty) => Right(ty)
    }

  /** Γ ⊢ e1 : a     Γ, x : a ⊢ e2 : b
    * --------------------------------
    *     Γ ⊢ let x = e1 in e2 : b
    */
  def ruleLet(
      bindings: List[Binding],
      body: Expr,
      env: Environment
  ): Either[TyError, Ty] = {
    val loc = bindings.foldLeft[Environment](env) { (env, binding) =>
      env.declare(
        binding.name,
        Ty.of(binding, env).fold(err => return Left(err), ok => ok)
      )
    }

    Ty.of(body, loc)
  }

  /** Γ, x : a ⊢ e : b
    * ----------------
    * Γ ⊢ λx.e : a → b
    *
    * Note that there could be multiple e's as arguments and multiple a's as
    * types. An empty e is typed as unit.
    */
  def ruleFunc(
      decl: Function,
      body: Expr,
      env: Environment
  ): Either[TyError, Ty] = {
    val (loc, intys) =
      decl.args.foldRight[(Environment, List[Ty])]((env, List.empty)) {
        case (arg, (env, tys)) =>
          arg.typ match {
            // TODO Once inference is complete this can go away.
            case None => return Left(UnknownTyOf(arg.name.lexeme))
            case Some(typ) =>
              env.get(typ.name.lexeme) match {
                case None     => return Left(UnknownTy(typ.name.lexeme))
                case Some(ty) => (env.declare(arg.name.lexeme, ty), ty :: tys)
              }
          }
      }

    val bod = Ty.of(body, loc) match {
      case Left(err) => return Left(err)
      case Right(ty) => ty
    }

    val outty = decl.rtyp match {
      case None => bod
      case Some(typ) =>
        env.get(typ.name.lexeme) match {
          case None => return Left(UnknownTy(typ.name.lexeme))
          case Some(ret) =>
            if (ret.sub(bod))
              bod
            else
              return Left(UnexpectedTy(body, ret, bod))
        }
    }

    val links =
      if (intys.isEmpty)
        List(TyUnit, outty)
      else
        List(TyTuple(intys), outty)

    Right(TyLambda(links))
  }

  /** Γ ⊢ e1 : a → b     Γ ⊢ e2 : a
    * -----------------------------
    *        Γ ⊢ e1 e2 : b
    *
    * Note that there could be multiple e2's as arguments An empty e2 is also
    * allowed and is typed as unit.
    *
    * TODO e1 is not really an expression but a name we lookup instead. This
    * should be updated to handle expressions instead of just function lookups.
    */
  def ruleApp(
      app: App,
      fn: String,
      args: List[Expr],
      env: Environment
  ): Either[TyError, Ty] = {
    val tyargs = args.map(of(_, env).fold(err => return Left(err), ok => ok))

    (env.get(fn), TyTuple(tyargs)) match {
      case (Some(TyLambda(fnargs :: ret :: Nil)), tyargs) =>
        if (!tyargs.sub(fnargs))
          Left(UnexpectedTy(app, fnargs, tyargs))
        else
          Right(ret)

      case (Some(ty), tyargs) =>
        Left(UnexpectedTy(app, TyLambda(tyargs.fields), ty))
      case (None, _) => Left(UnknownVariableTy(fn))
    }
  }

  /** See ruleLet for type rule
    */
  def ruleVar(
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

  /** Γ ⊢ e1 : bool     Γ ⊢ e2 : a     Γ ⊢  e3 : a
    * --------------------------------------------
    *      Γ ⊢ cond e1 then e2 else e3 : a
    */
  def ruleCond(expr: Cond, env: Environment): Either[TyError, Ty] = {
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

/** Γ, holds the scoped environment
  */
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
        Ty.NameTyBool -> TyBool,
        Ty.NameTyInt -> TyInt,
        Ty.NameTyReal -> TyReal,
        Ty.NameTyStr -> TyStr
      )
    )
}
