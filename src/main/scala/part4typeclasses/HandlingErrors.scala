package part4typeclasses

import cats.{Applicative, Monad}

import scala.util.Try

object HandlingErrors {

  trait MyApplicativeError[M[_], E] extends Applicative[M] {
    def raiseError[A](e: E): M[A]
    def handleErrorWith[A](ma: M[A])(func: E => M[A]): M[A]
    def handleError[A](ma: M[A])(func: E => A): M[A] = handleErrorWith(ma)(e => pure(func(e)))
  }

  trait MyMonadError[M[_], E] extends MyApplicativeError[M, E]  with Monad[M] {
    def ensure[A](ma: M[A])(error: E)(pred: A => Boolean): M[A]
  }

  import cats.MonadError
  import cats.instances.either._
  type ErrorOr[A] = Either[String, A]
  val eitherMonadError: MonadError[ErrorOr, String] = MonadError[ErrorOr, String]
  val success: ErrorOr[Int] = eitherMonadError.pure(42)                 // Right(42)
  val failure: ErrorOr[Int] = eitherMonadError.raiseError[Int]("Boom!") // Left(Boom)

  val handled: ErrorOr[Int] = eitherMonadError.handleError(failure) {
    case "Badness" => 44
    case _         => 42
  }
  val handle2: ErrorOr[Int] = eitherMonadError.handleErrorWith(failure) {
    case "Badness" => eitherMonadError.pure(42)
    case _         => eitherMonadError.raiseError("Boom")
  }
  val filtered: ErrorOr[Int] = eitherMonadError.ensure(success)("Number too small")(_ > 100)

  import cats.instances.try_._
  val exception = new RuntimeException("Boom")
  val pureException: Try[Int] = MonadError[Try, Throwable].raiseError(exception)

  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  import cats.ApplicativeError
  import cats.instances.list._
  val valAppError: ApplicativeError[ErrorsOr, List[String]] = ApplicativeError[ErrorsOr, List[String]]

  import cats.syntax.applicative._
  import cats.syntax.applicativeError._
  val extendedSuccess: ErrorsOr[Int] = 42.pure[ErrorsOr]
  val extendedError: ErrorsOr[Int] = List("Boom").raiseError[ErrorsOr, Int]
  val recoveredError: ErrorsOr[Int] = extendedError.recover {
    case _ => 42
  }

  import cats.syntax.monadError._
  val testedSuccess: ErrorOr[Int] = success.ensure("Boom")(_ > 100)

  def main(args: Array[String]): Unit = {
    println(s"success:         $success")
    println(s"failure:         $failure")
    println(s"handled:         $handled")
    println(s"handle2:         $handle2")
    println(s"filtered:        $filtered")
    println(s"pureException:   $pureException")
    println(s"extendedSuccess: $extendedSuccess")
    println(s"extendedError:   $extendedError")
    println(s"recoveredError:  $recoveredError")
    println(s"testedSuccess:   $testedSuccess")
  }
}
