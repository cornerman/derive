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
  def canWarn = new SuccessCompileMatcher().canWarn
  def warn = new SuccessCompileMatcher().withWarnings
  def warn(msgs: String*) = new SuccessCompileMatcher().withWarnings(msgs: _*)
  def abort = new SuccessCompileMatcher().withErrors
  def abort(msgs: String*) = new SuccessCompileMatcher().withErrors(msgs: _*)
}
