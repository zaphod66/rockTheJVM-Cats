package part4typeclasses

object Applicatives {

  import cats.Applicative

  import cats.instances.list._
  val listApplicative: Applicative[List] = Applicative[List]
  val l1: Seq[Int] = listApplicative.pure(2)

  import cats.instances.option._
  val optionApplicative: Applicative[Option] = Applicative[Option]
  val o1: Option[Int] = optionApplicative.pure(2)

  import cats.syntax.applicative._
  val l2: Seq[Int] = 2.pure[List]
  val o2: Option[Int] = 2.pure[Option]

  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val validatedApplicative: Applicative[ErrorsOr] = Applicative[ErrorsOr]
  val v1: ErrorsOr[Int] = validatedApplicative.pure(2)

  def ap[F[_], A, B](ff: F[A => B])(fa: F[A])(implicit applicative: Applicative[F]): F[B] = applicative.ap(ff)(fa)   // given, already implemented
  def productWithApplicatives[F[_], A, B](fa: F[A], fb: F[B])(implicit applicative: Applicative[F]): F[(A, B)] = {
    val functionWrapper: F[B => (A, B)] = applicative.map(fa)(a => (b: B) => (a, b))
    ap(functionWrapper)(fb)
  }

  def main(args: Array[String]): Unit = {
    println(s"l1: $l1")
    println(s"o1: $o1")
    println(s"l2: $l2")
    println(s"o2: $o2")
    println(s"v1: $v1")
  }
}
