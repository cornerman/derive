package helpers

import compiler._
import scala.reflect.runtime.universe._
import org.specs2.mutable.Specification
import org.specs2.matcher._

trait CompileSpec extends Specification {
  implicit def TreeToWith(t: Tree): With = With(t)
  implicit def StringToWith(code: String): With = With(q"$code")
  implicit def CompileMatcherToCompileTreeMatcher(matcher: Matcher[CompileResult]) = new CompileTreeMatcher(matcher)

  def compile = new SuccessCompileMatcher()
  def canWarn = compile.canWarn
  def warn = compile.withWarnings
  def warn(msg: String, msgs: String*) = compile.withWarnings(msg, msgs: _*)
  def warn(msg: Matcher[String], msgs: Matcher[String]*) = compile.withWarnings(msg, msgs: _*)
  def abort = compile.withErrors
  def abort(msg: String, msgs: String*) = compile.withErrors(msg, msgs: _*)
  def abort(msg: Matcher[String], msgs: Matcher[String]*) = compile.withErrors(msg, msgs: _*)
}
