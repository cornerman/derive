package compiler

import scala.tools.reflect.FrontEnd
import scala.reflect.runtime.universe.Tree
import scala.reflect.api.Position

sealed trait Message {
  val pos: Position
  val msg: String
  val severity: String
  override def toString = s"[$severity] line ${pos.line}: $msg"
}
object Message {
  def apply(reporter: FrontEnd)(info: reporter.Info) = info match {
    case reporter.Info(pos, msg, reporter.INFO) => Info(pos, msg)
    case reporter.Info(pos, msg, reporter.WARNING) => Warning(pos, msg)
    case reporter.Info(pos, msg, reporter.ERROR) => Error(pos, msg)
  }
}

case class Info(pos: Position, msg: String) extends Message { val severity = "INFO" }
case class Warning(pos: Position, msg: String) extends Message { val severity = "WARN" }
case class Error(pos: Position, msg: String) extends Message { val severity = "ERROR" }
