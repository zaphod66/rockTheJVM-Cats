package part2abstractMath

object Functors {

  import cats.Functor
  import cats.instances.list._

  val listFunctor = Functor[List]

  val l1 = List(1, 2, 3)
  val l2 = listFunctor.map(l1)(_ + 1)

  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)

  trait Tree[+T]
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object treeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(v) => Leaf(f(v))
      case Branch(v, l, r) => Branch(f(v), map(l)(f), map(r)(f))
    }
  }

  import cats.instances.option._
  import cats.instances.either._

  import cats.syntax.functor._

  def do10x2[F[_]: Functor](container: F[Int]): F[Int] = container.map(_ * 10)

  def main(args: Array[String]): Unit = {
    println(s"list: ${do10x(l2)}")
    println(s"list: ${do10x(Option(10))}")
    val e1: Either[String, Int] = Right(21)
    println(s"list: ${do10x(e1)}")
    val t1: Tree[Int] = Branch(1, Branch(2, Leaf(3), Leaf(4)), Leaf(5))
    println(s"tree: ${do10x2(t1)}")
    println(s"tree: ${t1.map(_ + 1)}")
  }
}
