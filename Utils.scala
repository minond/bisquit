package xyz.minond.bisquit.utils

import scala.reflect.ClassTag

/** Compile time _and_ runtime type checking. The returned value will be
  * annotated with the expected type, which ensuring that at runtime the
  * value type checks and type errors are handled.
  *
  *   ensure[String, Int](3344, "expected a number!") // Right(34)
  *   ensure[String, Int]("34", "expected a number!") // Left("expected a number!")
  */
def ensure[L, R: ClassTag](value: Any, left: => L): Either[L, R] =
  value match {
    case ok : R => Right(ok)
    case _ => Left(left)
  }

object Implicits {
  import scala.language.implicitConversions

  implicit class Eithers[L, R](val eithers: List[Either[L, R]]) {
    /** Allows for the conversion of a list of eithers into an either with a
      * list. For example, a list version of a function that returns an either
      * would in turn return a list of eithers:
      *
      *   doIt  -> X -> Either[L, R]
      *   doIts -> List[X] -> List[Either[L, R]]
      *   squished -> List[Either[L, R]] -> Either[L, List[R]]
      *
      * The `squished` method lets you convert `List[Either[L, R]]` into an
      * `Either[L, List[R]]`.
      */
    def squished(): Either[L, List[R]] =
      eithers.foldLeft[Either[L, List[R]]](Right(List())) {
        (acc, x) =>
          acc.flatMap(xs => x.map(_ +: xs))
      }
  }
}
