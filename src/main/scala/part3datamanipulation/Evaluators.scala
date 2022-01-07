package part3datamanipulation

object Evaluators {

  import cats.Eval

  val instantEval: Eval[Int] = Eval.now {    // instan evaluation
    println("computing now")
    42
  }

  val redoEval: Eval[Int] = Eval.always {
    println("computing always")
    43
  }

  val laterEval: Eval[Int] = Eval.later {
    println("computing later")
    44
  }

  val composedEval1: Eval[Int] = instantEval.flatMap(v1 => laterEval.map(v2 => v1 + v2))
  val composedEval2: Eval[Int] = for {
    v1 <- redoEval
    v2 <- laterEval
  } yield v1 + v2

  val memEval: Eval[Int] = redoEval.memoize

  val composedEval3: Eval[String] = Eval
    .always { println("Step1"); "step1"}
    .map { step1 => println("Step2"); s"$step1 and then step2" }
    .memoize
    .map { step2 => println("Step3"); s"$step2 and then step3"}

  def defer[T](eval: => Eval[T]): Eval[T] = Eval.later{ () }.flatMap{ _ => eval }

  val deferredEval: Eval[Int] = defer { Eval.now{ println("now!!"); 42 } }

  def reverseList[T](list: List[T]): List[T] =
    if (list.isEmpty) list
    else reverseList(list.tail) :+ list.head

  // stacksafe due to defer
  def reverseListEval[T](list: List[T]): Eval[List[T]] =
    if (list.isEmpty) Eval.now(list)
    else defer(reverseListEval(list.tail).map(l => l :+ list.head))

  def main(args: Array[String]): Unit = {
    println("====================")
    println(s"instantEval     = ${instantEval.value}")
    println(s"instantEval     = ${instantEval.value}")
    println("--------------------")
    println(s"redoEval        = ${redoEval.value}")
    println(s"redoEval        = ${redoEval.value}")
    println("--------------------")
    println(s"laterEval       = ${laterEval.value}")
    println(s"laterEval       = ${laterEval.value}")
    println("--------------------")
    println(s"composedEval1   = ${composedEval1.value}")
    println(s"composedEval1   = ${composedEval1.value}")
    println("--------------------")
    println(s"composedEval2   = ${composedEval2.value}")
    println(s"composedEval2   = ${composedEval2.value}")
    println("--------------------")
    println(s"memEval         = ${memEval.value}")
    println(s"memEval         = ${memEval.value}")
    println("--------------------")
    println(s"composedEval3   = ${composedEval3.value}")
    println(s"composedEval3   = ${composedEval3.value}")
    println("--------------------")
    println(s"deferredEval    = ${deferredEval.value}")
    println(s"deferredEval    = ${deferredEval.value}")
    println("--------------------")
    val list = List(1, 2, 3, 4)
    println(s"reverseList      = ${reverseList(list)}")
    println(s"reverseListEval  = ${reverseListEval((1 to 10000).toList).value.take(6)}")
  }
}
