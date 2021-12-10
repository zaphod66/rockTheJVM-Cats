package part1intro

object TCVariance {

  import cats.Eq
  import cats.instances.int._
  import cats.instances.option._
  import cats.syntax.eq._

  val aComp: Boolean = Option(2) === Option(3)

  // val invalidComp = Some(2) === None  // Eq[Some[Int]] not found

  // variance
  class Animal
  class Cat extends Animal

  // covariant type: subtyping is propagated to the generic type
  class Cage[+T]
  val cage: Cage[Animal] = new Cage[Cat]  // Cat <: Animal => Cage[Cat} <: Cage[Animal]

  // contravariant type: subtyping is propagated backwards to the generic type
  class Vet[-T]
  // a veterinarian of animals can also heal cats. (a 'better' veterinarian)
  val aVet: Vet[Cat] = new Vet[Animal] // Cat <: Animal => Cage[Animal} <: Cage[Cat]

  // rule of thumb: "HAS a T" => covariant, "ACTS on T" => contravariant
  // variance affect how TC instance are being fetched

  // contravariant TC
  trait SoundMaker[-T]
  implicit object AnimalSoundMaker extends SoundMaker[Animal]
  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("sound")

  makeSound[Animal]
  makeSound[Cat]

  // rule 1: contravariant TCs can use the superclass instances if nothing is available strictly for that type

  // covariant TC
  trait AnimalShow[+T] {
    def show: String
  }
  implicit object generalAnimalShow extends AnimalShow[Animal] {
    override def show: String = "animals everywhere"
  }
  implicit object catsAnimalShow extends AnimalShow[Cat] {
    override def show: String = "so many cats"
  }
  def organizeShow[T](implicit ev: AnimalShow[T]): String = ev.show

  def main(args: Array[String]): Unit = {
    println(organizeShow[Cat])
  }
}
