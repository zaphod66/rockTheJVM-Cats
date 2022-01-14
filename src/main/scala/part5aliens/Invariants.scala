package part5aliens

import cats.Monoid

object Invariants {

  trait Crypt[A] { self =>
    def encrypt(value: A): String
    def decrypt(encrypt: String): A

    def imap[B](back: B => A, forth: A => B): Crypt[B] = new Crypt[B] {
      override def encrypt(b: B): String = self.encrypt(back(b))
      override def decrypt(encrypt: String): B = forth(self.decrypt(encrypt))
    }
  }

  def encrypt[A](value: A)(implicit crypt: Crypt[A]): String = crypt.encrypt(value)
  def decrypt[A](encr: String)(implicit crypt: Crypt[A]): A = crypt.decrypt(encr)

  implicit val caesarCypher: Crypt[String] = new Crypt[String] {
    override def encrypt(value: String): String = value.map(c => (c + 2).toChar)
    override def decrypt(encr: String): String = encr.map(c => (c - 2).toChar)
  }

  // How to support int, doubles, options??
  implicit val doubleCrypt: Crypt[Double] = caesarCypher.imap(_.toString, _.toDouble)

  implicit val optionStringCrypt: Crypt[Option[String]] = caesarCypher.imap(_.getOrElse(""), Option(_))

  implicit def optionCrypt[T](implicit c: Crypt[T], m: Monoid[T]): Crypt[Option[T]] =
    c.imap(_.getOrElse(m.empty), Option(_))

  import cats.Invariant
  import cats.Show
  import cats.instances.int._
  val intShow: Show[Int] = Show[Int]
  implicit val optionIntShow: Show[Option[Int]] = Invariant[Show].imap(intShow)(Option(_))(_.getOrElse(0))

  import cats.syntax.invariant._
  val optionIntShow2: Show[Option[Int]] = intShow.imap(Option(_))(_.getOrElse(0))

  trait MyInvariant[F[_]] {
    def imap[A, B](fa: F[A])(forth: A => B)(back: B => A): F[B]
  }

  trait MyContravariant[F[_]] extends MyInvariant[F] { self =>
    def contramap[A, B](fa: F[A])(back: B => A): F[B]

    override def imap[A, B](fa: F[A])(forth: A => B)(back: B => A): F[B] = self.contramap(fa)(back)
  }

  trait MyFunctor[F[_]] extends MyInvariant[F] { self =>
    def map[A, B](fa: F[A])(forth: A => B): F[B]

    override def imap[A, B](fa: F[A])(forth: A => B)(back: B => A): F[B] = self.map(fa)(forth)
  }

  def main(args: Array[String]): Unit = {
    val plain1 = "Plain text"
    val encrypted1 = encrypt(plain1)
    val decrypted1 = decrypt[String](encrypted1)

    println(s""">$plain1< => >$encrypted1< => >$decrypted1<""")

    val plain2 = Math.PI
    val encrypted2 = encrypt(plain2)
    val decrypted2 = decrypt[Double](encrypted2)

    println(s""">$plain2< => >$encrypted2< => >$decrypted2<""")

    val plain3 = Option("Optional String")
    val encrypted3 = encrypt(plain3)
    val decrypted3 = decrypt[Option[String]](encrypted3)

    println(s""">$plain3< => >$encrypted3< => >$decrypted3<""")

    val plain4 = Option(Math.E)
    val encrypted4 = encrypt(plain4)
    val decrypted4 = decrypt[Option[Double]](encrypted4)

    println(s""">$plain4< => >$encrypted4< => >$decrypted4<""")

    import cats.syntax.show._
    println(s""">${Option(42).show}<""")
  }
}
