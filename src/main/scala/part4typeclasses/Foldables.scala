package part4typeclasses

import cats.{Eval, Monoid}

object Foldables {

  /// implement in terns of foldLeft & foldRight
  object ListExercises {
    def map[A, B](l: List[A])(f: A => B): List[B] = l.foldRight(List.empty[B])((a, acc) => f(a) :: acc)
    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l.foldLeft(List.empty[B])((acc, a) => acc ++ f(a))
    def filter[A](l: List[A])(pred: A => Boolean): List[A] = l.foldRight(List.empty[A])((a, acc) => if (pred(a)) a :: acc else acc)
    def combineAll[A](l: List[A])(implicit monoid: Monoid[A]): A = l.foldLeft(monoid.empty)(monoid.combine)
  }


  def main(args: Array[String]): Unit = {
    import ListExercises._

    val list = (1 to 10).toList
    println(s"""map:        ${map(list)(i => s"<$i>")}""")
    println(s"""flatMap:    ${flatMap(list)(i => List(s"<$i>", s">$i<"))}""")
    println(s"""filter:     ${filter(list)(i => i % 2 == 0)}""")
    println(s"""combineAll: ${combineAll(list)}""")

    import cats.Foldable
    import cats.instances.list._
    println(s"Foldable[List]:   ${Foldable[List].foldLeft(list, 0)(_ + _)}")
    import cats.instances.option._
    println(s"Foldable[Option]: ${Foldable[Option].foldLeft(Option(2), 40)(_ + _)}")

    // foldRight is stacksafe regardless of your context
    val sumRight  = Foldable[List].foldRight(list, Eval.now(0)) { (a, acc) => acc.map(_ + a) }
    val sumList   = Foldable[List].combineAll(list)
    val sumConcat = Foldable[List].foldMap(list)(_.toString)

    val intsNested = List(Vector(1, 2, 3), Vector(4, 5, 6))
    val sumNested  = (Foldable[List] compose Foldable[Vector]).combineAll(intsNested)

    import cats.syntax.foldable._
    val sumExtended = list.combineAll

    println(s"sumRight:    ${sumRight.value}")
    println(s"sumList:     $sumList")
    println(s"sumConcat:   $sumConcat")
    println(s"sumNested:   $sumNested")
    println(s"sumExtended: $sumExtended")
  }
}
