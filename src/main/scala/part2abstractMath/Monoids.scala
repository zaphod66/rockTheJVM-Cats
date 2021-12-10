package part2abstractMath

object Monoids {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._

  val numbers = (1 to 1000).toList
  // |+| is always associative
  val sumLeft = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)

  def generalFold[T](list: List[T], zero: T)(implicit semigroup: Semigroup[T]) =
    list.foldLeft(zero)(_ |+| _)

  import cats.Monoid
  val intMonoid = Monoid[Int]
  val sumMonoid = generalFold(numbers, intMonoid.empty)

  def foldMonoid[T](list: List[T])(implicit monoid: Monoid[T]): T = list.foldLeft(monoid.empty)(_ |+| _)

  val phoneBooks = List(
    Map(
      "Alice" -> 235,
      "Bob" -> 647
    ),
    Map(
      "Charlie" -> 342,
      "Daniel" -> 345
    ),
    Map(
      "Tina" -> 123
    )
  )

  import cats.instances.option._

  val o1 = Monoid[Option[Int]].combine(Some(2), Option.empty[Int])
  val o2 = Monoid[Option[Int]].combine(Some(2), Option(40))

  import cats.instances.map._

  case class ShoppingCart(items: List[String], total: Double)

  import cats.instances.list._
  import cats.instances.double._

  implicit val shoppingCartMonoid = Monoid.instance[ShoppingCart](ShoppingCart(List.empty[String], 0.0),  (sc1, sc2) =>
    ShoppingCart(sc1.items |+| sc2.items, sc1.total |+| sc2.total)
  )

  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart = foldMonoid(shoppingCarts)

  val scs1 = List(ShoppingCart(List("Phone"), 600.0), ShoppingCart(List("Item1", "Item2"), 23.5))

  def main(args: Array[String]): Unit = {
    println(s"left: $sumLeft, right: $sumRight, general: $sumMonoid")
    println(s"o1: $o1, o2: $o2")
    println(s"ints: ${foldMonoid(numbers)}")
    println(s"""strings: ${foldMonoid(List("I ", "like ", "monoids."))}""")
    println(s"maps: ${foldMonoid(phoneBooks)}")
    println(s"carts: ${foldMonoid(scs1)}")
  }
}
