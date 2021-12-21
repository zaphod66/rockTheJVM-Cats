package part2abstractMath

object CustomMonads {

  import cats.Monad

  import cats.syntax.applicative._  // pure
  import cats.syntax.flatMap._      // flatMap

  implicit object OptionMonad extends Monad[Option] {
    override def pure[A](a: A): Option[A] = Option(a)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case None           => None
      case Some(Left(a))  => tailRecM(a)(f)
      case Some(Right(b)) => Some(b)
    }
  }

  // tailRecM is used for interate... methods on Monads. Like OptionMonad.iterateUntil()

  type Identity[T] = T
  val aNumber: Identity[Int] = 42

  implicit object IdentityMonad extends Monad[Identity] {
    override def pure[A](a: A): Identity[A] = a

    override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)

    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
      case Left(a)  => tailRecM(a)(f)
      case Right(b) => b
    }
  }

  sealed trait Tree[+A]
  final case class Leaf[+A](value: A) extends Tree[A]
  final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    def leaf[A](a: A): Tree[A] = Leaf(a)
    def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)
  }

  implicit object TreeMonad extends Monad[Tree] {
    import Tree._

    override def pure[A](a: A): Tree[A] = leaf(a)

    // not stacksafe
    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(a)      => f(a)
      case Branch(l, r) => branch(flatMap(l)(f), flatMap(r)(f))
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      // not stacksafe
      def stackRec(t: Tree[Either[A, B]]): Tree[B] = t match {
        case Leaf(Left(a))  => stackRec(f(a))
        case Leaf(Right(b)) => leaf(b)
        case Branch(l, r) => branch(stackRec(l), stackRec(r))
      }

      def tailRec(todo: List[Tree[Either[A, B]]], expanded: List[Tree[Either[A, B]]], done: List[Tree[B]]): Tree[B] =
        if (todo.isEmpty) done.head
        else todo.head match {
          case Leaf(Left(v))       => tailRec(f(v) :: todo.tail, expanded, done)
          case Leaf(Right(b))      => tailRec(todo.tail, expanded, leaf(b) :: done)
          case node @ Branch(l, r) =>
            if (!expanded.contains(node)) {
              tailRec(r :: l :: todo, node :: expanded, done)
            } else {
              val newL = done.head
              val newR = done.tail.head
              val newB = branch(newL, newR)
              tailRec(todo.tail, expanded, newB :: done.drop(2))
            }
        }

//      stackRec(f(a))
      tailRec(List(f(a)), List.empty, List.empty)
    }
  }

  def main(args: Array[String]): Unit = {
    import Tree._
    val t1: Tree[Int] = branch(leaf(10), leaf(20))
    val t2 = TreeMonad.flatMap(t1)(v => branch(leaf(v + 1), leaf(v + 2)))

    println(s"t1: $t1")
    println(s"t2: $t2")
  }
}
