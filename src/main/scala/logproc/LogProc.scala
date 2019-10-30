package logproc

import java.time.{LocalDateTime, ZoneOffset}

import scala.util.matching.Regex

object LogProc extends App {

  val service = "inventory"
  val logGroup = "/ecs/platform-uat"
  val eventTime = System.currentTimeMillis() - (1000 * 60 * 5) // 5 mins ago

  val callPattern: Regex = "----> (\\w+(?:\\.\\w+)+)\\((.*)\\)".r
  val returnPattern: Regex = "<----\\[(\\d+\\.\\d+)] (\\w+(?:\\.\\w+)+) => (.*)".r

  val logs: Seq[LogStatement] = CloudWatch.logs(service, logGroup, None)
  val funcLogs: Seq[FunctionLog] = parseLogs(logs)
  val threadMap: Map[String,Seq[FunctionLog]] = mapThreads(funcLogs)

  threadMap.foreach { case (thread, logs) =>
    var indent = "";
    println(thread)
    logs.flatMap {
      case FunctionCall(timestamp, thread, function, args) =>
        indent += "  "
        Seq.empty
      case FunctionReturn(timestamp, thread, function, returnVal, duration) =>
        indent = indent.substring(2)
        Seq(s"$indent $function $duration")
    }.reverse.foreach(println)
  }


  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////



  def profile(head: FunctionCall, tail: Seq[FunctionLog]): FunctionTree = tail match {
    case (ret:FunctionReturn) :: Nil if(head.function == ret.function) =>
      Leaf(head.function)

    case (child:FunctionCall) :: rest =>
      Branch(head.function, Seq(profile(child, rest)))

    case _ => ??? // All other cases are errors
  }

  def mapThreads(funcLogs: Seq[FunctionLog]): Map[String, Seq[FunctionLog]] = funcLogs
    .groupBy(_.thread)
    .map {
      case (thread, logs) => (thread, logs.sortBy(_.timestamp.toEpochSecond(ZoneOffset.UTC)))
    }

  def parseLogs(logs: Seq[LogStatement]): Seq[FunctionLog] = logs.flatMap { log =>
    val timestamp = log.timestamp
    val thread = log.thread

    log.message match {
      case callPattern(func, args) =>
        Seq(FunctionCall(timestamp, thread, func, args))
      case returnPattern(elapsedTime, func, returnVal) =>
        Seq(FunctionReturn(timestamp, thread, func, returnVal, elapsedTime.toDouble))
      case _ =>
        Seq.empty
    }
  }
}

