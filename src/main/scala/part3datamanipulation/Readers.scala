package part3datamanipulation

import cats.Id
import cats.data.Kleisli

object Readers {

  /*
    - configuration file => initial data structure
    - a DB layer
    - an HTTP layer
    - a business logic layer
   */

  case class Config(dbUser: String, dbPass: String, host: String, port: Int, nThreads: Int, replyTo: String)
  case class DbConnection(user: String, pass: String) {
    def getOderStatus(oderId: Long): String = s"$oderId dispatched"
    def getLastOrderId(user: String): Long = 542643
  }

  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("server started")
  }

  val config: Config = Config("Bob", "password", "localhost", 1234, 8, "support@store.com")

  import cats.data.Reader

  val dbReader: Reader[Config, DbConnection] = Reader(conf => DbConnection(conf.dbUser, conf.dbPass))
  val orderStatusReader: Reader[Config, String] = dbReader.map(dbConn => dbConn.getOderStatus(4211L))

  def getLastOrderStatusReader(user: String) = dbReader
    .map(_.getLastOrderId(user))
    .flatMap(lastOrderId => dbReader.map(_.getOderStatus(lastOrderId)))

  def getLastOrderStatus(user: String): String = {
    getLastOrderStatusReader(user).run(config)
  }

  case class EmailService(replyTo: String) {
    def sendEmail(address: String, content: String) = s"From: $replyTo, to: $address >>> $content"
  }

  def emailUser(user: String, email: String) = {
    val emailServiceReader: Reader[Config, EmailService] = Reader(conf => EmailService(conf.replyTo))

    val emailReader: Reader[Config, String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(user))
      orderStatus <- dbReader.map(_.getOderStatus(lastOrderId))
      emailService <- emailServiceReader
    } yield (emailService.sendEmail(email, s"Your last order has status: $orderStatus"))

    emailReader.run(config)
  }
  /*
    Pattern:
    1. you create an initial data structure
    2. you create a reader which specifies how that data structure will be manipulated later
    3. you can then map & flatMap the reader to produce derived information
    4. when you need the final piece of info, you call `run` on the reader with the initial data structure
   */

  def main(args: Array[String]): Unit = {
    val dbConnection: DbConnection = dbReader.run(config)
    val orderStatus: String = orderStatusReader.run(config)

    println(s"dbConnection: $dbConnection")
    println(s"orderStatus:  $orderStatus")
    println(s"""getLastOrderStatus: ${getLastOrderStatus("Alan")}""")
    println(s"""emailUser: ${emailUser("Alice", "aliceß@gmail.com")}""")
  }
}
