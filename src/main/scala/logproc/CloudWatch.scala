package logproc

import java.time.LocalDateTime
import sys.process._
import scala.language.postfixOps

import org.json4s.native.JsonMethods.parse
import logproc.Serde._

object CloudWatch {

  def logs(service: String, logGroup: String, eventTime: Option[Long]): Seq[LogStatement] =
    logStreams(service, logGroup, eventTime).map(logStatements(logGroup, _)).getOrElse(Seq.empty)

  def logStatements(logGroup: String, log: CloudwatchLogStream): Seq[LogStatement] = {
    val logEventsJson: String = s"aws logs get-log-events --log-group-name $logGroup --log-stream-name ${log.logStreamName}" !!

    val logEvents: Seq[CloudwatchLogEvent] = parse(logEventsJson).extract[CloudwatchLogEvents].events

    logEvents.map { event =>
      parse(event.message).transformField {
        case ("@timestamp", value) => ("timestamp", value)
        case ("thread_name", value) => ("thread", value)
      }.extract[LogStatement]
    }
  }

  def logStreams(service: String, logGroup: String, eventTime: Option[Long]): Option[CloudwatchLogStream] = {
    val logStreamsJson = s"aws logs describe-log-streams --log-group-name $logGroup --order-by LastEventTime --descending" !!

    val logStreamsMap = parse(logStreamsJson).extract[Map[String,Seq[CloudwatchLogStream]]]
    val logStreams: Seq[CloudwatchLogStream] = logStreamsMap("logStreams")

    logStreams.filter { log =>
      log.firstEventTimestamp.nonEmpty && log.lastEventTimestamp.nonEmpty
    }.find{ log =>
      val namePrefix: String = s"$service/$service"
      val prefixMatches = log.logStreamName.startsWith(namePrefix)

      val eventTimeMatches = eventTime.forall { time =>
        log.firstEventTimestamp.get <= time && log.lastEventTimestamp.get >= time
      }

      prefixMatches && eventTimeMatches
    }
  }
}

case class CloudwatchLogStream(logStreamName: String, firstEventTimestamp: Option[Long], lastEventTimestamp: Option[Long])
case class CloudwatchLogEvents(events: Seq[CloudwatchLogEvent])
case class CloudwatchLogEvent(timestamp: Long, message: String)
case class LogStatement(timestamp: LocalDateTime, thread: String, message: String)