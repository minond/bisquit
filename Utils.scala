package xyz.minond.bisquit.utils

import scala.reflect.ClassTag

def ensure[R: ClassTag, L, V](value: V, left: => L): Either[L, R] =
  value match {
    case ok : R => Right(ok)
    case _ => Left(left)
  }

object Eithers {
  import scala.language.implicitConversions

  implicit def listOfEithersToEithers[L, R](eithers: List[Either[L, R]]): Container[L, R] =
    Container(eithers)

  class Container[L, R](val eithers: List[Either[L, R]]) {
    def squished(): Either[L, List[R]] = {
      val acc: Either[L, List[R]] = Right(List())
      eithers.foldLeft(acc) {
        (acc, x) =>
          acc.flatMap(xs => x.map(_ +: xs))
      }
    }
  }
}
