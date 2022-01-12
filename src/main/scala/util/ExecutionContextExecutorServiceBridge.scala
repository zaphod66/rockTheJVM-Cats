package util

import java.util
import java.util.Collections
import java.util.concurrent.AbstractExecutorService
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}
import scala.concurrent.duration.TimeUnit

object ExecutionContextExecutorServiceBridge {
  def apply(ec: ExecutionContext): ExecutionContextExecutorService = ec match {
    case null => throw null
    case eces: ExecutionContextExecutorService => eces
    case other => new AbstractExecutorService with ExecutionContextExecutorService {
      override def prepare(): ExecutionContext = other
      override def isShutdown = false
      override def isTerminated = false
      override def shutdown(): Unit = ()
      override def shutdownNow(): util.List[Runnable] = Collections.emptyList[Runnable]
      override def execute(runnable: Runnable): Unit = other execute runnable
      override def reportFailure(t: Throwable): Unit = other reportFailure t
      override def awaitTermination(length: Long,unit: TimeUnit): Boolean = false
    }
  }
}