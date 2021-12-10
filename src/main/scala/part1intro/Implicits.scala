package part1intro

object Implicits {

  // implicit classes (can have only one argument)
  implicit class Greeter(name: String) {
    def greet: String = s"Hello $name"
  }

  val greeting: String = "Peter".greet

  // importing implicit conversions in scope
  import scala.concurrent.duration._
  val oneSec: FiniteDuration = 1.second

  // implicit args and vals
  case class Person(perName: String)
  case class Cat(catName: String)

  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  def listToJson[T](list: List[T])(implicit ser: JSONSerializer[T]): String =
    list.map(v => ser.toJson(v)).mkString("[", ", ", "]")

  implicit def oneArgCaseClassSerializer[T <: Product]: JSONSerializer[T] = new JSONSerializer[T] {
    override def toJson(value: T): String =
      s"""
         |{"${value.productElementName(0)}": "${value.productElement(0)}"}
      """.stripMargin.trim
  }

  val personsJson = listToJson(List(Person("Alice"), Person("Bob")))
  val catsJson = listToJson(List(Cat("Garfield"), Cat("Tom")))

  def main(args: Array[String]): Unit = {
    println(personsJson)
    println(catsJson)
  }

}
