package logproc

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import org.json4s
import org.json4s.{CustomSerializer, DefaultFormats, Formats, JString}

object Serde {
  val formatter: DateTimeFormatter = DateTimeFormatter.ISO_DATE_TIME
  implicit val defaultFormats: Formats = DefaultFormats + new CustomSerializer[LocalDateTime](_ => (
    {
      case JString(s) => LocalDateTime.parse(s,formatter)
    },{
    case d: LocalDateTime => JString(formatter.format(d))
  }))
}
