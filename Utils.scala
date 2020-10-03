package bisquit
package utils

import scala.reflect.ClassTag


/** Rebuilds a map by passing each key though a function.
 *
 *  {{{
 *  scala> import bisquit.utils._
 *  scala> val chars: Map[Char, Int] = rekey(Map(97 -> 97)) { _.toChar }
 *  val chars: Map[Int, Char] = Map(a -> 97)
 *  }}}
 */
def rekey[K1, K2, V](orig: Map[K1, V])(f: K1 => K2): Map[K2, V] =
  orig.foldLeft[Map[K2, V]](Map()) {
    case (acc, (k, v)) => acc ++ Map(f(k) -> v)
  }

/** Rebuilds a map by passing each value though a function.
 *
 *  {{{
 *  scala> import bisquit.utils._
 *  scala> val chars: Map[Int, Char] = remap(Map(1 -> 97)) { _.toChar }
 *  val chars: Map[Int, Char] = Map(1 -> a)
 *  }}}
 */
def remap[K, V1, V2](orig: Map[K, V1])(f: V1 => V2): Map[K, V2] =
  orig.foldLeft[Map[K, V2]](Map()) {
    case (acc, (k, v)) => acc ++ Map(k -> f(v))
  }

/** Applies [[f]] over every value in [[orig]]. [[f]] returns an [[Either]]
 *  type and [[formap]] flattens the return of every call into a single
 *  [[Left]] or [[Right]] value.
 *
 *  {{{
 *  scala> import bisquit.utils._
 *  scala> import bisquit.ast._
 *  scala> import bisquit.runtime._
 *  scala> formap(Map(1 -> Int(43))) { v => eval(pass1(v)) }
 *  val res0: Either[bisquit.runtime.RuntimeError, Map[Int, bisquit.ast.Value]] = Right(Map(1 -> Int(43)))
 *  scala> formap(Map(1 -> Id("not_found"))) { v => eval(pass1(v)) }
 *  val res1: Either[bisquit.runtime.RuntimeError, Map[Int, bisquit.ast.Value]] = Left(LookupError(Id(not_found)))
 *  }}}
 */
def formap[K, V, L, R](orig: Map[K, V])(f: V => Either[L, R]): Either[L, Map[K, R]] =
  orig.foldLeft[Either[L, Map[K, R]]](Right(Map())) {
    case (Left(l), _) => Left(l)
    case (Right(acc), (k, v)) =>
      f(v).flatMap { r =>
        Right(acc ++ Map(k -> r))
      }
  }

def formap[K, V, L, R](orig: Map[K, V])(f: (K, V) => Either[L, R]): Either[L, Map[K, R]] =
  orig.foldLeft[Either[L, Map[K, R]]](Right(Map())) {
    case (Left(l), _) => Left(l)
    case (Right(acc), (k, v)) =>
      f(k, v).flatMap { r =>
        Right(acc ++ Map(k -> r))
      }
  }

/** Compile time _and_ runtime type checking. The returned value will be
 *  annotated with the expected type, which ensuring that at runtime the
 *  value type checks and type errors are handled.
 *
 *  {{{
 *  scala> import bisquit.utils._
 *  scala> ensure[String, Int](3344, "expected a number!")
 *  val res0: Either[String, Int] = Right(3344)
 *  scala> ensure[String, Int]("34", "expected a number!")
 *  val res1: Either[String, Int] = Left(expected a number!)
 *  }}}
 */
def ensure[L, R: ClassTag](value: Any, left: => L): Either[L, R] =
  value match {
    case ok : R => Right(ok)
    case _ => Left(left)
  }

object Implicits {
  import scala.language.implicitConversions

  implicit class Lists[T](items: List[T]) {
    def ensureItems[L, V: ClassTag](l: T => L): Either[L, List[V]] =
      Right(items.map[V] { item =>
        if item.isInstanceOf[V]
        then item.asInstanceOf[V]
        else return Left(l(item))
      })
  }

  implicit class Eithers[L, R](val eithers: List[Either[L, R]]) {
    /** Lets you convert a [[List[Either[L, R]]]] into an [[Either[L, List[R]]]].
     *
     *  {{{
     *  scala> import scala.language.implicitConversions
     *  scala> import bisquit.utils.Implicits.Eithers
     *  scala> import bisquit.ast._
     *  scala> import bisquit.runtime._
     *  scala> List(Int(43)).map { eval(_) }
     *  val res0: List[Either[bisquit.runtime.RuntimeError, bisquit.ast.Value]] = List(Right(Int(43)))
     *  scala> List(Int(43)).map { eval(_) }.squished()
     *  val res1: Either[bisquit.runtime.RuntimeError, List[bisquit.ast.Value]] = Right(List(Int(43)))
     *  scala> List(Id("not_found")).map { eval(_) }.squished()
     *  val res2: Either[bisquit.runtime.RuntimeError, List[bisquit.ast.Value]] = Left(LookupError(Id(not_found)))
     *  }}}
     */
    def squished(): Either[L, List[R]] =
      eithers.foldLeft[Either[L, List[R]]](Right(List())) {
        (acc, x) =>
          acc.flatMap(xs => x.map(xs :+ _))
      }
  }

  implicit class Iterators[L, R](val eithers: Iterator[Either[L, R]]) {
    def squished(): Either[L, List[R]] =
      eithers.toList.foldLeft[Either[L, List[R]]](Right(List())) {
        (acc, x) =>
          acc.flatMap(xs => x.map(xs :+ _))
      }
  }
}
