package part4typeclasses

import cats.{Applicative, Apply}

object FlapMaps {

  trait MyFlatMap[M[_]] extends Apply[M]{
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    def ap[A, B](mf: M[A => B])(ma: M[A]): M[B] =
      flatMap(ma)(a => map(mf)(f => f(a)))
  }

  trait MyMonad[M[_]] extends Applicative[M] with MyFlatMap[M] {
    override def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => pure(f(a)))
  }

  import cats.FlatMap
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def getPairs[M[_]: FlatMap, A, B](ma: M[A], mb: M[B]): M[(A, B)] = for {
    a <- ma
    b <- mb
  } yield (a, b)

  def main(args: Array[String]): Unit = {

  }
}
