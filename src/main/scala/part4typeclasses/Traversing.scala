package part4typeclasses

import cats.{Applicative, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Traversing {

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val servers: List[String] = List("server-ci.google.com", "server-stating.google.com", "server-prod.google.com")
  def getBandwidth(hostname: String): Future[Int] = Future(hostname.length * 80)

  val allBandwidths: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) { (acc, host) =>
    val bandFuture = getBandwidth(host)
    for {
      accBW <- acc
      band  <- bandFuture
    } yield accBW :+ band
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

  def main(args: Array[String]): Unit = {
    println(s"l1: $l1")
    println(s"l2: $l2")
  }
}
