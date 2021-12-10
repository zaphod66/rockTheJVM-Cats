package recap

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

object Essentials {

  // starting with Scala 2.13 the global executionContext implementation has changed,
  // to save some Future spawning overhead. As such nested Futures will not run in parallel,
  // unless we use the blocking API.
  // This is maybe a wrong tradeoff, so we construct our own EC
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  def main(args: Array[String]): Unit = {

  }
}
