import java.time.LocalDateTime

package object logproc {

  sealed trait FunctionLog {
    val timestamp: LocalDateTime
    val thread: String
    val function: String
  }

  case class FunctionCall(timestamp: LocalDateTime, thread: String, function: String, args: String) extends FunctionLog
  case class FunctionReturn(timestamp: LocalDateTime, thread: String, function: String, returnVal: String, duration: Double) extends FunctionLog

  case class FunctionProfile(
    call: FunctionCall,
    ret: FunctionReturn,
    children: Seq[FunctionProfile]
  )

  trait FunctionTree
  case class Leaf(name: String) extends FunctionTree
  case class Branch(name: String, children: Seq[FunctionTree]) extends FunctionTree
}
