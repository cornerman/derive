package helpers

import compiler._
import org.specs2.matcher._
import scala.reflect.runtime.universe._

class EqualsMatcher(expected: Tree) extends Matcher[CompileResult] {
  import ErrorMessage._, TreeComparison._

  override def apply[S <: CompileResult](expectable: Expectable[S]): MatchResult[S] = {
    expectable.value match {
      case CompileSuccess(source, generated, _) =>
        result(equalsSnippet(generated, expected), "Are equal", compareError(source, generated, expected), expectable)
      case CompileFailure(source, msgs) => MatchFailure("", compileError(source, msgs), expectable)
    }
  }
}

class ContainsMatcher(snippets: Seq[ExpectedCode]) extends Matcher[CompileResult] {
  import ErrorMessage._, TreeComparison._
  import scala.compat.Platform.EOL

  override def apply[S <: CompileResult](expectable: Expectable[S]): MatchResult[S] = {
    expectable.value match {
      case CompileSuccess(source, generated, _) =>
        val failures = unexpectedSnippets(source, generated, snippets)
        val failMsg = failures.map(failure => compareError(source, generated, failure)).mkString(EOL)
        result(failures.isEmpty, "Contains all", failMsg, expectable)
      case CompileFailure(source, msgs) => MatchFailure("", compileError(source, msgs), expectable)
    }
  }
}

trait CompileMatcher[M <: CompileMatcher[M]] extends Matcher[CompileResult] {
  import ErrorMessage._

  val hasValidErrors: Seq[Error] => Boolean
  val hasValidWarnings: Seq[Warning] => Boolean
  def copy(hasValidErrors: Seq[Error] => Boolean, hasValidWarnings: Seq[Warning] => Boolean): M

  private def isSuccess(compiled: CompileResult) = hasValidErrors(compiled.errors) && hasValidWarnings(compiled.warnings)

  override def apply[S <: CompileResult](expectable: Expectable[S]): MatchResult[S] = {
    val compiled = expectable.value
    isSuccess(compiled) match {
      case true => MatchSuccess("Compiles", "", expectable)
      case _ => MatchFailure("", compileError(compiled.source, compiled.messages), expectable)
    }
  }

  def canWarn = copy(hasValidErrors, _ => true)
  def withWarning = copy(hasValidErrors, _.size == 1)
  def withWarnings = copy(hasValidErrors, _.nonEmpty)
  def withWarning(msg: String) = withWarnings(msg)
  def withWarnings(msgs: String*) = copy(hasValidErrors, _.map(_.msg) == msgs)

  def withError = new FailureCompileMatcher(_.size == 1, hasValidWarnings)
  def withErrors = new FailureCompileMatcher(_.nonEmpty, hasValidWarnings)
  def withError(msg: String) = withErrors(msg)
  def withErrors(msgs: String*) = new FailureCompileMatcher(_.map(_.msg) == msgs, hasValidWarnings)
}

class SuccessCompileMatcher(val hasValidWarnings: Seq[Warning] => Boolean = _.isEmpty) extends CompileMatcher[SuccessCompileMatcher] {
  val hasValidErrors: Seq[Error] => Boolean = _.isEmpty

  def copy(hasValidErrors: Seq[Error] => Boolean, hasValidWarnings: Seq[Warning] => Boolean) = new SuccessCompileMatcher(hasValidWarnings)

  def to(tree: Tree) = this and (new EqualsMatcher(tree))
  def containing(snippets: ExpectedCode*) = this and (new ContainsMatcher(snippets))
}

class FailureCompileMatcher(val hasValidErrors: Seq[Error] => Boolean, val hasValidWarnings: Seq[Warning] => Boolean) extends CompileMatcher[FailureCompileMatcher] {
  def copy(hasValidErrors: Seq[Error] => Boolean, hasValidWarnings: Seq[Warning] => Boolean) = new FailureCompileMatcher(hasValidErrors, hasValidWarnings)
}
