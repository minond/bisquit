package bisquit
package utils

import scala.reflect.ClassTag

/** Rebuilds a map by passing each value though a function.
 *
 *    val chars: Map[Int, Char] = remap(Map(1 -> 1)) { _.toChar }
 */
def remap[K, V1, V2](orig: Map[K, V1])(f: V1 => V2): Map[K, V2] =
  orig.foldLeft[Map[K, V2]](Map()) {
    case (acc, (k, v)) => acc ++ Map(k -> f(v))
  }

/** Applies [[f]] over every value in [[orig]]. [[f]] returns an [[Either]]
 *  type and [[formap]] flattens the return of every call into a single
 *  [[Left]] or [[Right]] value.
 */
def formap[K, V, L, R](orig: Map[K, V])(f: V => Either[L, R]): Either[L, Map[K, R]] =
  orig.foldLeft[Either[L, Map[K, R]]](Right(Map())) {
    case (Left(l), _) => Left(l)
    case (Right(acc), (k, v)) =>
      f(v).flatMap { r =>
        Right(acc ++ Map(k -> r))
      }
  }

/** Compile time _and_ runtime type checking. The returned value will be
 *  annotated with the expected type, which ensuring that at runtime the
 *  value type checks and type errors are handled.
 *
 *    ensure[String, Int](3344, "expected a number!") // Right(34)
 *    ensure[String, Int]("34", "expected a number!") // Left("expected a number!")
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
     *  list. For example, a list version of a function that returns an either
     *  would in turn return a list of eithers:
     *
     *    doIt  -> X -> Either[L, R]
     *    doIts -> List[X] -> List[Either[L, R]]
     *    squished -> List[Either[L, R]] -> Either[L, List[R]]
     *
     * Lets you convert a [[List[Either[L, R]]]] into an [[Either[L, List[R]]]].
     */
    def squished(): Either[L, List[R]] =
      eithers.foldLeft[Either[L, List[R]]](Right(List())) {
        (acc, x) =>
          acc.flatMap(xs => x.map(xs :+ _))
      }
  }
}
