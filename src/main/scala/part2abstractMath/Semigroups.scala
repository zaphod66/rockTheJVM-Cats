package part2abstractMath

object Semigroups {

  import cats.Semigroup
  import cats.instances.int._
  val naturalIntSemigroup = Semigroup[Int]
  val intCombination = naturalIntSemigroup.combine(2, 40)

  import cats.instances.string._
  val naturalStringSemigroup = Semigroup[String]
  val stringCombination = naturalStringSemigroup.combine("I love ", "cats")

  def reduceInts(l: List[Int]): Int = l.reduce(naturalIntSemigroup.combine)
  def reduceString(l: List[String]): String = l.reduce(naturalStringSemigroup.combine)

  def reduceThings[T](l: List[T])(implicit sg: Semigroup[T]): T = l.reduce(sg.combine)

  case class Expense(id: Long, amount: Double)
  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance[Expense] { (exp1, exp2) =>
    Expense(math.max(exp1.id, exp2.id), exp1.amount + exp2.amount)
  }

  import cats.syntax.semigroup._
  val s1 = 1 |+| 2
  val s2 = Expense(3, 23.2) |+| Expense(2, 23)

  def reduceThings2[T : Semigroup](l: List[T]): T = l.reduce(_ |+| _)

  def main(args: Array[String]): Unit = {
    println(intCombination)
    println(stringCombination)

    val numbers = (0 to 10).toList
    val strings = List("I'm ", "starting ", "to ", "like ", "semigroups.")
    val options = numbers.map(Option(_))
    println(reduceInts(numbers))
    println(reduceThings(numbers))
    println(reduceThings(strings))
    import cats.instances.option._
    println(reduceThings(options))

    val expenses = (0 to 10).toList.map(i => Expense(i, i.toDouble))
    println(reduceThings(expenses))
  }
}
