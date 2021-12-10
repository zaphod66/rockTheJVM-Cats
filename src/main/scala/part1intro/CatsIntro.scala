package part1intro

class CatsIntro {

  // Eq

  // part 1 - type class import
  import cats.Eq

  // part 2 - import TC instances
  import cats.instances.int._

  // part 3 - use the TC API
  val intEquality: Eq[Int] = Eq[Int]

  val aTypeSaveEquality: Boolean = intEquality.eqv(2, 3)

  // part 4 - use extension methods
  import cats.syntax.eq._

  val anotherComparision: Boolean = 2 === 3
  val yetAnother: Boolean = 2 =!= 3

  // part 5 - extending the TC operations to composite types
  import cats.instances.list._  // bring Eq[List[Int]] into scope

  val aListComp: Boolean = List(2) === List(3)

  // part 6 - create a TC for a custom type
  case class ToyCar(model: String, price: Double)

  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar] { (c1, c2) =>
    c1.price == c2.price
  }

  val toyCarComp: Boolean = ToyCar("Ferarri", 29.99)  === ToyCar("Porsche", 29.99)
}
