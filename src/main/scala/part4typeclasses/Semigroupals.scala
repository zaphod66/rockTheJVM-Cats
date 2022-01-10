package part4typeclasses

object Semigroupals {

  trait MySemigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  import cats.Semigroupal
  import cats.instances.option._

  val optionSemigroupal = Semigroupal[Option]
  val anOption = optionSemigroupal.product(Some(123), Some("a string"))

  import cats.instances.list._
  val aTupledList = Semigroupal[List].product(List(1, 2), List('A', 'B'))

  import cats.Monad

  def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] =
    monad.flatMap(fa)(a => monad.map(fb)(b => (a, b)))

  val bTupledList = productWithMonads(List(1, 2), List('A', 'B'))

  import cats.syntax.flatMap._
  import cats.syntax.functor._
  def productWithMonads2[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  val cTupledList = productWithMonads2(List(1, 2), List('A', 'B'))

  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal = Semigroupal[ErrorsOr]
  val validatedInvalids = validatedSemigroupal.product(
    Validated.Invalid(List("Error 1", "Error 2")),
    Validated.invalid(List("Error 3"))
  )

  // follows the monad associativity law: m.flatMap(f).flatMao(g) == m.flatMap(x => f(x).flatMap(g))
  // so it `short-circuits`
  type EitherErrorsOr[T] = Either[List[String], T]
  val eitherSemigroupal = Semigroupal[EitherErrorsOr]
  val eitherInvalids = eitherSemigroupal.product(
    Left(List("Error 1", "Error 2")),
    Left(List("Error 3"))
  )

  val zipListSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa.zip(fb)
  }

  def main(args: Array[String]): Unit = {
    println(s"aTupledList: $aTupledList")
    println(s"bTupledList: $bTupledList")
    println(s"cTupledList: $cTupledList")

    println(s"validatedInvalids: $validatedInvalids")
    println(s"eitherInvalids:    $eitherInvalids")

    println(s"yipTupledList: ${zipListSemigroupal.product(List(1, 2), List('A', 'B'))}")
  }
}
