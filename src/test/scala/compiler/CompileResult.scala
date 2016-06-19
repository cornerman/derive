package compiler

import scala.tools.reflect.FrontEnd
import scala.reflect.api.Position
import scala.reflect.runtime.universe.{Tree, NoPosition}

sealed trait CompileResult {
  val source: Tree
  val messages: Seq[Message]

  def infos = messages collect { case m: Info => m }
  def warnings = messages collect { case m: Warning => m }
  def errors = messages collect { case m: Error => m }
}

case class CompileSuccess(source: Tree, generated: Tree, messages: Seq[Message]) extends CompileResult {
  assert(errors.isEmpty)
}
object CompileSuccess {
  def apply(source: Tree, generated: Tree, reporter: FrontEnd): CompileResult = {
    CompileSuccess(source, generated, reporter.infos.map(Message(reporter)).toSeq)
  }
}

case class CompileFailure(source: Tree, messages: Seq[Message]) extends CompileResult {
  assert(errors.nonEmpty)
}
object CompileFailure {
  def apply(source: Tree, e: Throwable): CompileResult = {
    CompileFailure(source, Seq(Error(NoPosition, e.getMessage)))
  }
}
