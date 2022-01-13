package part5aliens

import cats.data.Reader

object Kleislis {

  val func1: Int => Option[String] = x => if (x % 2 == 0) Some(s"$x is even") else None
  val func2: Int => Option[Int] = x => Some(x * 2)

  import cats.data.Kleisli
  import cats.instances.option._
  val func1K: Kleisli[Option, Int, String] = Kleisli(func1)
  val func2K: Kleisli[Option, Int, Int] = Kleisli(func2)
  val func3K: Kleisli[Option, Int, String] = func2K andThen func1K

  val func4K: Kleisli[Option, Int, Int] = func2K.map(_ * 2)

  import cats.Id
  type IdKleisli[A, B] = Kleisli[Id, A, B]  // => Reader
  val times2: Reader[Int, Int] = Kleisli[Id, Int, Int](x => x * 2)
  val plus4: Reader[Int, Int] = Kleisli[Id, Int, Int](x => x + 4)
  val composed1: Reader[Int, Int] = times2.flatMap(t2 => plus4.map(p4 => t2 + p4))
  val composed2: Reader[Int, Int] = for {
    t2 <- times2
    p4 <- plus4
  } yield t2 + p4

  def main(args: Array[String]): Unit = {
    println(s"composed1(3): ${composed1(3)}")
    println(s"composed2(3): ${composed2(3)}")
  }
}
