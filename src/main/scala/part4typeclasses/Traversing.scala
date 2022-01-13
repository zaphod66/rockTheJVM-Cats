package part4typeclasses

import cats.{Applicative, Foldable, Functor, Monad}

import java.util.concurrent.Executors
import scala.concurrent.duration.Duration.Inf
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutorService, Future}

object Traversing {

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val servers: List[String] = List("server-ci.google.com", "server-stating.google.com", "server-prod.google.com")
  def getBandwidth(hostname: String): Future[Int] = Future(hostname.length * 80)

  val allBandwidths: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) { (accM, host) =>
    val bandM = getBandwidth(host)
    for {
      acc   <- accM
      band  <- bandM
    } yield acc :+ band
  }

  val allBandwidthTraverse: Future[List[Int]] = Future.traverse(servers)(getBandwidth)
  val allBandwidthSequence: Future[List[Int]] = Future.sequence(servers.map(getBandwidth))

  import cats.syntax.applicative._
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  def listTraverseM[F[_]: Monad, A, B](list: List[A])(f: A => F[B]): F[List[B]] = list.foldLeft(List.empty[B].pure[F]) { (accM, a) =>
    val elemM = f(a)
    for {
      acc  <- accM
      elem <- elemM
    } yield acc :+ elem
  }

  import cats.syntax.apply._
  def listTraverseA[F[_]: Applicative, A, B](list: List[A])(f: A => F[B]): F[List[B]] = list.foldLeft(List.empty[B].pure[F]) { (accM, a) =>
    val elemM = f(a)
    (accM, elemM).mapN(_ :+ _)
  }

  def listSequence1[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] = list.foldRight(List.empty[A].pure) { (fa, acc) =>
    (fa, acc).mapN(_ :: _)
  }
  def listSequence2[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] = listTraverseA(list)(identity)

  import cats.instances.vector._
  val l1: Vector[List[Int]] = listSequence2(List(Vector(1, 2), Vector(3, 4)))
  val l2: Vector[List[Int]] = listSequence2(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))

  import cats.instances.option._
  def filterAsOption(list: List[Int])(p: Int => Boolean): Option[List[Int]] =
    listTraverseA[Option, Int, Int](list)(n => Some(n).filter(p))

  val l3: Option[List[Int]] = filterAsOption(List(2, 4, 6))(_ % 2 == 0)
  val l4: Option[List[Int]] = filterAsOption(List(1, 2, 3))(_ % 2 == 0)

  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  def filterAsValidated(list: List[Int])(p: Int => Boolean): ErrorsOr[List[Int]] =
    listTraverseA[ErrorsOr, Int, Int](list)(n => if (p(n)) Validated.Valid(n) else Validated.Invalid(List(s"predicate failed for $n")))

  val l5: ErrorsOr[List[Int]] = filterAsValidated(List(2, 4, 6))(_ % 2 == 0)
  val l6: ErrorsOr[List[Int]] = filterAsValidated(List(1, 2, 3))(_ % 2 == 0)

  trait MyTraverse[M[_]] extends Foldable[M] with Functor[M] {
    def traverse[F[_]: Applicative, A, B](ma: M[A])(f: A => F[B]): F[M[B]]
    def sequence[F[_]: Applicative, A](ma: M[F[A]]): F[M[A]] = traverse(ma)(identity)

    import cats.Id
    def map[A, B](ma: M[A])(f: A => B): M[B] = traverse[Id, A, B](ma)(f)
  }

  import cats.Traverse
  val allBandwidthsCats: Future[List[Int]] = Traverse[List].traverse(servers)(getBandwidth)

  import cats.syntax.traverse._
  val allBandwidthsCats2: Future[List[Int]] = servers.traverse(getBandwidth)

  def main(args: Array[String]): Unit = {
    println(s"allBandwidthTraverse: ${Await.result(allBandwidthTraverse, Inf)}")
    println(s"allBandwidthSequence: ${Await.result(allBandwidthSequence, Inf)}")
    println(s"l1: $l1")
    println(s"l2: $l2")
    println(s"l3: $l3")
    println(s"l4: $l4")
    println(s"l5: $l5")
    println(s"l6: $l6")
    println(s"allBandwidthsCats:    ${Await.result(allBandwidthsCats, Inf)}")
    println(s"allBandwidthsCats2:   ${Await.result(allBandwidthsCats2, Inf)}")

    ec.asInstanceOf[ExecutionContextExecutorService].shutdown()
  }
}
