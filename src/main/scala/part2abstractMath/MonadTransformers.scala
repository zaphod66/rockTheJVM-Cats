package part2abstractMath

import part2abstractMath.MonadTransformers.canWithstandSurge

import java.util.concurrent.Executors
import scala.concurrent.duration.Duration.Inf
import scala.concurrent.{Await, ExecutionContext, Future}

object MonadTransformers {

  def summAllOptions(values: List[Option[Int]]): Int = ???

  import cats.data.OptionT
  import cats.instances.list._

  val listIntOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option.empty[Char]))
  val liostTuples: OptionT[List, (Int, Char)] = for {
    c <- listCharOptions
    n <- listIntOptions
  } yield (n, c)

  import cats.data.EitherT

  val listEithers: EitherT[List, String, Int] = EitherT(List(Left("err1"), Right(42), Right(43), Left("err2")))

  // Exercise

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val bandwidths = Map (
    "server1" ->  50,
    "server2" -> 300,
    "server3" -> 170,
  )
  type AsyncResponse[T] = EitherT[Future, String, T]

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
    case None    => EitherT[Future, String, Int](Future(Left[String, Int](s"Server $server unreachable")))
    case Some(b) => EitherT[Future, String, Int](Future(Right[String, Int](b)))
  }

  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = {
    for {
      b1 <- getBandwidth(s1)
      b2 <- getBandwidth(s2)
    } yield b1 + b2 > 250
  }

  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] = {
    canWithstandSurge(s1, s2).transform {
      case Left(s) => Left(s)
      case Right(b) => if (b)
        Right(s"Servers $s1, $s2 can withstand the surge.")
      else
        Left(s"Servers $s1, $s2 can not withstand the surge")
    }
  }

  def main(args: Array[String]): Unit = {
    println(liostTuples.value)
    println("-------------")
    val cf = canWithstandSurge("server1", "server2").value
    val cc = Await.result(cf, Inf)
    println(s"""canWithstandSurge(s1, s2) = $cc""")

    def util(s1: String, s2: String): Either[String, String] = {
      val rf = generateTrafficSpikeReport(s1, s2).value
      Await.result(rf, Inf)
    }

    val rc1 = util("server1", "server2")
    println(s"""generateTrafficSpikeReport(s1, s2) = $rc1""")
    val rc2 = util("server1", "server3")
    println(s"""generateTrafficSpikeReport(s1, s2) = $rc2""")
    val rc3 = util("server1", "server4")
    println(s"""generateTrafficSpikeReport(s1, s2) = $rc3""")

  }
}
