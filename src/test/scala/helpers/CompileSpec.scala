package helpers

import compiler._
import scala.reflect.runtime.universe.Tree
import org.specs2.mutable.Specification
import org.specs2.matcher._

class CompileMatcher(hasValidErrors: Seq[Error] => Boolean = _.isEmpty, hasValidWarnings: Seq[Warning] => Boolean = _.isEmpty) extends Matcher[CompileResult] {
  import ErrorMessage._

  def isSuccess(compiled: CompileResult) = hasValidErrors(compiled.errors) && hasValidWarnings(compiled.warnings)

  override def apply[S <: CompileResult](expectable: Expectable[S]): MatchResult[S] = {
    val compiled = expectable.value
    isSuccess(compiled) match {
      case true => MatchSuccess("Compiles", "", expectable)
      case _ => MatchFailure("", compileError(compiled.source, compiled.messages), expectable)
    }
  }

  def canWarn = new CompileMatcher(hasValidErrors, _ => true)
  def withWarning = new CompileMatcher(hasValidErrors, _.size == 1)
  def withWarnings = new CompileMatcher(hasValidErrors, _.nonEmpty)
  def withWarning(msg: String) = withWarnings(msg)
  def withWarnings(msgs: String*) = new CompileMatcher(hasValidErrors, _.map(_.msg) == msgs)

  def withError = new CompileMatcher(_.size == 1, hasValidWarnings)
  def withErrors = new CompileMatcher(_.nonEmpty, hasValidWarnings)
  def withError(msg: String) = withErrors(msg)
  def withErrors(msgs: String*) = new CompileMatcher(_.map(_.msg) == msgs, hasValidWarnings)
}

trait CompileSpec extends Specification {
  implicit class CompilableTree(tree: Tree) {
    def compile = Compiler(tree)
  }

  def compile = new CompileMatcher
  def warn: CompileMatcher = new CompileMatcher().withWarnings
  def warn(msgs: String*) = new CompileMatcher().withWarnings(msgs: _*)
  def abort: CompileMatcher = new CompileMatcher().withErrors
  def abort(msgs: String*) = new CompileMatcher().withErrors(msgs: _*)
}
