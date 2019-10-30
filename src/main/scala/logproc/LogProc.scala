package logproc

import java.text.SimpleDateFormat
import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZoneOffset}

import scala.io.Source

object LogProc extends App {
  val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS")


  val callPatternString: String = "----> ((?:\\w|\\$)+(?:\\.\\w+)+)\\((.*)\\)"
  val returnPatternString: String = "<----\\[(\\d+\\.\\d+)] ((?:\\w|\\$)+(?:\\.(?:\\w|\\$)+)+) => (.*)"

  val callPattern = callPatternString.r
  val returnPattern = returnPatternString.r

  val callPatternLog = s"(.*) DEBUG .* \\[(.*)] .* $callPatternString".r
  val returnPatternLog = s"(.*) DEBUG .* \\[(.*)] .* $returnPatternString".r

  val funcLogs: Seq[FunctionLog] = parseLocalLogs("logs.txt") // <--------------------------- log file here
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

  def parseAWSLogs(service: String, logGroup: String): Seq[FunctionLog] = {
//    val service = "inventory"
//    val logGroup = "/ecs/platform-uat"
    parseLogs(CloudWatch.logs(service, logGroup, None))
  }

  def parseLocalLogs(file: String): Seq[FunctionLog] = {
    val logFile = Source.fromFile(file, "utf-8")
    logFile.getLines
      .filter(line => (line.contains("---->") || line.contains("<----")))
      .map {
        case callPatternLog(timestampString, thread, func, args) =>
          val timestamp = LocalDateTime.parse(timestampString,formatter)
          FunctionCall(timestamp, thread, func, args)
        case returnPatternLog(timestampString, thread, elapsedTime, func, returnVal) =>
          val timestamp = LocalDateTime.parse(timestampString,formatter)
          FunctionReturn(timestamp, thread, func, returnVal, elapsedTime.toDouble)
      }.toSeq
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
}

