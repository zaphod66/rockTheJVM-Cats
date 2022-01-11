package part4typeclasses

import cats.{Functor, Semigroupal}

object Applys {

  trait MyApply[F[_]] extends Functor[F] with Semigroupal[F] {
    override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
      def functionWrapper: F[B => (A, B)] = map(fa)(a => (b: B) => (a, b))
      ap(functionWrapper)(fb)
    }

    def map2[A, B, C](tuple: (F[A], F[B]))(f: (A, B) => C): F[C] = {
      val tupleWrapper: F[(A, B)] = product(tuple._1, tuple._2)
      map(tupleWrapper)(ab => f(ab._1, ab._2))
    }

    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]   // fundamental for Apply
  }

  trait MyApplicative[F[_]] extends MyApply[F] {
    def pure[A](a: A): F[A]   // fundamental for Applicative
  }

  import cats.Apply
  import cats.instances.option._

  val optionApply: Apply[Option] = Apply[Option]
  val funcApp: Option[Int] = optionApply.ap(Option((x: Int) => x + 1))(Option(41))   // Some(42)

  import cats.syntax.apply._
  val tupleOfOption: (Option[Int], Option[Int], Option[Int]) = (Option(1), Option(2), Option(3))
  val optionOfTuple: Option[(Int, Int, Int)] = tupleOfOption.tupled   // by virtue of product
  val sumOption: Option[Int] = tupleOfOption.mapN(_ + _ + _)   // Some(6


  def main(args: Array[String]): Unit = {

  }
}
