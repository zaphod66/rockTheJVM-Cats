package part3datamanipulation

object Writers {

  import cats.data.Writer

  // 1 - define
  val aWriter: Writer[List[String], Int]  = Writer(List.empty[String], 42)

  // 2 - manipulate
  val aWriter1: Writer[List[String], Int] = aWriter.map(_ + 1)                     // value change, logs stays
  val aWriter2: Writer[List[String], Int]= aWriter.mapWritten(_ :+ "log entry")   // value stays, logs change
  val aWriter3: Writer[List[String], Int]= aWriter.bimap(_ :+ "log entry", _ + 1) // value change, logs change
  val aWriter4: Writer[List[String], Int]= aWriter.mapBoth { (logs, value) =>
    (logs :+ s"$value changed", value + 1)
  }                                                     // value change, logs change

  val writerA = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB = Writer(Vector("Log B1"), 40)

  import cats.instances.vector.catsKernelStdMonoidForVector
  val writerC = for {
    va <- writerA
    vb <- writerB
  } yield va + vb

  // 3 - get results
  val value = aWriter4.value
  val logs  = aWriter4.written
  val (l, v) = aWriter4.run

  def countAndLog(n: Int): Writer[Vector[String], Int] = {

    def go(w: Writer[Vector[String], Int]): Writer[Vector[String], Int] =
      if (w.value <= 0)
        w.mapWritten(_.+:("starting"))
      else
        go(w.mapBoth {(l, v) => (v.toString +: l, v - 1)})

    go(Writer(Vector.empty[String], n))
  }

  def countAndLog2(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("starting"), 0)
    else countAndLog2(n - 1).flatMap(_ => Writer(Vector(s"$n"), 0))
  }

  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val sum = naiveSum(n - 1)
      println(s"Computed sum(${n - 1}) = $sum")
      sum + n
    }
  }

  def writerSum(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("init"), 0)
    else for {
      _   <- Writer(Vector(s"Now at $n"), 0)
      sum <- writerSum(n - 1)
      _   <- Writer(Vector(s"Computed sum(${n - 1}) = $sum"), 0)
    } yield sum + n
  }

  def main(args: Array[String]): Unit = {
    println(s"writerC: ${writerC.run}")
    println(s"countAndLog:  ${countAndLog(10).written}")
    println(s"countAndLog2: ${countAndLog2(10).written}")

    println(s"naiveSum(5):   ${naiveSum(5)}")
    println(s"writerSum(5):  ${writerSum(5).run}")
  }
}
