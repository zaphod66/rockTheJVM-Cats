package part3datamanipulation

import cats.{Eval, Semigroup}
import cats.data.IndexedStateT

object States {

  import cats.data.State

  val countAndSay: State[Int, String] = State { current => (current + 1, s"current $current") }

  val incState: State[Int, String] = State((s: Int) => (s + 1, s"Incremented $s to ${s + 1}"))
  val mulState: State[Int, String] = State((s: Int) => (s * 5, s"$s multiplied with 5 to ${s * 5}"))
  val comState: State[Int, (String, String)] = incState.flatMap { res1 =>
    mulState.map(res2 => (res1, res2))
  }

  val func1: Int => (Int, String) = (s: Int) => (s + 1, s"Incremented $s to ${s + 1}")
  val func2: Int => (Int, String) = (s: Int) => (s * 5, s"$s multiplied with 5 to ${s * 5}")
  val func3: Int => (Int, String) = func1 andThen {
    case (s1, res1) =>
      val (s2, res2) = func2(s1)
      (s2, res1 + "," + res2)
  }

  case class ShoppingCart(items: List[String], total: Double)
  def addToCart(item: String, price: Double): State[ShoppingCart, Double] = State { sc =>
      val newTotal = sc.total + price
      (ShoppingCart(item :: sc.items, newTotal), newTotal)
    }

  val cart: State[ShoppingCart, Double] = for {
    _ <- addToCart("iPencil", 100)
    _ <- addToCart("iPad cover", 30)
    c <- addToCart("cable", 7)
  } yield c

  def inspect[A, B](f: A => B): State[A, B] = State(s => (s, f(s)))
  def get[A]: State[A, A] = State(s => (s, s))
  def set[A](value: A): State[A, Unit] = State((_: A) => (value, ()))
  def modify[A](f: A => A): State[A, Unit] = State(s => (f(s), ()))

  val prog: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 10)
    b <- get[Int]
    _ <- modify[Int](_ + 42)
    c <- inspect[Int, Int](_ + 2)
  } yield (a, b, c)

  def main(args: Array[String]): Unit = {
    println(s"countAndSay  = ${countAndSay.run(10).value}")
    println(s"comState     = ${comState.run(10).value}")
    println(s"comFunc      = ${func3(10)}")

    val sc1 = ShoppingCart(List("iPhone"), 1000.0)
    val State = addToCart("iPad", 1500)

    println(s"ShoppingCart = ${State.run(sc1).value}")
    println(s"cart         = ${cart.run(sc1).value}")

    println(s"prog         = ${prog.run(0).value}")
  }
}
