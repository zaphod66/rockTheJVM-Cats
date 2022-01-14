package part5aliens

import cats.Monoid

object Contravariants {

  trait Format[A] { self =>
    def format(value: A): String

    def contramap[B](func: B => A): Format[B] = new Format[B] {
      override def format(b: B): String = self.format(func(b))
    }
  }

  def format[A](value: A)(implicit f: Format[A]): String = f.format(value)

  implicit object StringFormat extends Format[String] {
    override def format(value: String): String = s""""$value""""
  }

  implicit object IntFormat extends Format[Int] {
    override def format(value: Int): String = value.toString
  }

  implicit object BooleanFormat extends Format[Boolean] {
    override def format(value: Boolean): String = if (value) "Y" else "N"
  }

  //problem: given a Format[SomeType], can we have a Format[Option.SomeType]]?
  implicit def getOptionFormat[T](implicit f: Format[T], m: Monoid[T]): Format[Option[T]] = f.contramap[Option[T]](_.getOrElse(m.empty))

  def contramap[A, T](func: A => T)(implicit f: Format[T]): Format[A] = new Format[A] {
    override def format(a: A): String = f.format(func(a))
  }

  import cats.Contravariant
  import cats.Show
  import cats.instances.int._
  val intShow: Show[Int] = Show[Int]
  val optionShow: Show[Option[Int]] = Contravariant[Show].contramap(intShow)(_.getOrElse(0))

  import cats.syntax.contravariant._
  val optionShow2: Show[Option[Int]] = intShow.contramap(_.getOrElse(0))
  
  implicit val booleanMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = true
    override def combine(x: Boolean, y: Boolean): Boolean = x && y
  }

  def main(args: Array[String]): Unit = {
    println(format("Already known"))
    println(format(42))
    println(format(true))
    println(format(Option("Already known")))
    println(format(Option(42)))
    println(format(Option(Option(42))))
    println(format(Option(true)))
  }
}
