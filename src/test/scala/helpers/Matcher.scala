package helpers

import compiler._
import org.specs2.matcher._
import org.specs2.execute.{Result, Success, Failure}
import scala.reflect.runtime.universe._
import scala.compat.Platform.EOL
import ErrorDescription._, TreeComparison._

class EqualsMatcher(expected: Tree) extends Matcher[CompileResult] {
  override def apply[S <: CompileResult](expectable: Expectable[S]): MatchResult[S] = {
    expectable.value match {
      case CompileSuccess(source, generated, _) =>
        result(equalsSnippet(generated, expected), "Are equal", compareError(source, generated, expected), expectable)
      case CompileFailure(source, msgs) => MatchFailure("", compileError(source, msgs), expectable)
    }
  }
}

class ContainsMatcher(snippets: Seq[ExpectedCode]) extends Matcher[CompileResult] {
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

class SeqMatcherOfMatchers[T](matchers: Seq[Matcher[T]]) extends Matcher[Seq[T]] {
  override def apply[S <: Seq[T]](expectable: Expectable[S]): MatchResult[S] = {
    val missingMatchers = matchers.drop(expectable.value.size)
    val missingValues = expectable.value.drop(matchers.size) // TODO exactly without order semantic?
    val results: Seq[MatchResult[T]] = matchers.zip(expectable.value).map { case (matcher, exp) =>
      matcher.apply(createExpectable(exp))
    }

    def failure[M](reason: String, missing: Seq[_])(m: M): MatchFailure[M] = {
      val misses = missing.mkString(EOL)
      MatchFailure("", reason + ":" + EOL + misses, createExpectable(m))
    }

    val missingResults = missingMatchers.map(failure("Got less values than expected, missing", missingMatchers)) ++ missingValues.map(failure("Got more values than expected", missingValues))

    MatchResult.sequence(results ++ missingResults).asInstanceOf[MatchResult[S]] // what else?
  }
}

class MessageMatcher[M <: Message](matcher: Matcher[String]) extends Matcher[M] {
  override def apply[S <: M](expectable: Expectable[S]): MatchResult[S] = {
    val result = matcher(createExpectable(expectable.value.msg))
    result.setExpectable(expectable).asInstanceOf[MatchResult[S]]
  }
}

class MessagesMatcher[M <: Message](matcher: Matcher[Seq[M]]) extends Matcher[M] {
  override def apply[S <: M](expectable: Expectable[S]): MatchResult[S] = {
    ???
  }
}

class ValueCheckMatcher[T](checker: ValueCheck[T]) extends Matcher[T] {
  override def apply[S <: T](expectable: Expectable[S]): MatchResult[S] = {
    val checked = checker.check(expectable.value)
    result(checked.isSuccess, checked.message, checked.expected, expectable)
  }
}

object CheckMatchers {
  //TODO: set messages
  def alwaysOk[T]: Matcher[T] = new ValueCheckMatcher(ValueCheck.alwaysOk)
  def isEmpty[T]: Matcher[Seq[T]] = AnyMatchers.beEmpty
  def once[T]: Matcher[Seq[T]] = TraversableMatchers.haveSize(1)
  def nonEmpty[T]: Matcher[Seq[T]] = isEmpty.not
}

trait CompileMatcher[M <: CompileMatcher[M]] extends Matcher[CompileResult] {
  val hasValidErrors: Matcher[Seq[Error]]
  val hasValidWarnings: Matcher[Seq[Warning]]
  def copy(hasValidErrors: Matcher[Seq[Error]], hasValidWarnings: Matcher[Seq[Warning]]): M

  override def apply[S <: CompileResult](expectable: Expectable[S]): MatchResult[S] = {
    val compiled = expectable.value
    val matchedMessages = Seq(hasValidErrors(createExpectable(compiled.errors)), hasValidWarnings(createExpectable(compiled.warnings))).map(_.setExpectable(expectable))
    MatchResult.sequence(matchedMessages).asInstanceOf[MatchResult[S]]
  }

  def canWarn = copy(hasValidErrors, hasValidWarnings = CheckMatchers.alwaysOk)
  def withWarning = copy(hasValidErrors, hasValidWarnings = CheckMatchers.once)
  def withWarnings = copy(hasValidErrors, hasValidWarnings = CheckMatchers.nonEmpty)
  def withWarning(msg: String): M = withWarnings(msg)
  def withWarning(msg: Matcher[String]) = withWarnings(msg)
  def withWarnings(msg: String, msgs: String*): M = withWarnings(AnyMatchers.beEqualTo(msg), msgs.map(m => AnyMatchers.beEqualTo(m)): _*)
  def withWarnings(msg: Matcher[String], msgs: Matcher[String]*) = copy(hasValidErrors, hasValidWarnings = new SeqMatcherOfMatchers((msg +: msgs).map(m => new MessageMatcher[Warning](m))))

  def withError = new FailureCompileMatcher(CheckMatchers.once, hasValidWarnings)
  def withErrors = new FailureCompileMatcher(CheckMatchers.nonEmpty, hasValidWarnings)
  def withError(msg: String): FailureCompileMatcher = withErrors(msg)
  def withError(msg: Matcher[String]) = withErrors(msg)
  def withErrors(msg: String, msgs: String*): FailureCompileMatcher = withErrors(AnyMatchers.beEqualTo(msg), msgs.map(m => AnyMatchers.beEqualTo(m)): _*)
  def withErrors(msg: Matcher[String], msgs: Matcher[String]*) = new FailureCompileMatcher(new SeqMatcherOfMatchers((msg +: msgs).map(m => new MessageMatcher[Error](m))), hasValidWarnings)
}

class SuccessCompileMatcher(val hasValidWarnings: Matcher[Seq[Warning]] = CheckMatchers.isEmpty) extends CompileMatcher[SuccessCompileMatcher] {
  val hasValidErrors: Matcher[Seq[Error]] = CheckMatchers.isEmpty

  def copy(hasValidErrors: Matcher[Seq[Error]], hasValidWarnings: Matcher[Seq[Warning]]) = new SuccessCompileMatcher(hasValidWarnings)

  def to(tree: Tree) = this and (new EqualsMatcher(tree))
  def containing(snippets: ExpectedCode*) = this and (new ContainsMatcher(snippets))
}

class FailureCompileMatcher(val hasValidErrors: Matcher[Seq[Error]], val hasValidWarnings: Matcher[Seq[Warning]]) extends CompileMatcher[FailureCompileMatcher] {
  def copy(hasValidErrors: Matcher[Seq[Error]], hasValidWarnings: Matcher[Seq[Warning]]) = new FailureCompileMatcher(hasValidErrors, hasValidWarnings)
}

class CompileTreeMatcher(matcher: Matcher[CompileResult]) extends Matcher[Tree] {
  override def apply[S <: Tree](expectable: Expectable[S]): MatchResult[S] = {
    val compiled = Compiler(expectable.value)
    val result = matcher(createExpectable(compiled))
    result.setExpectable(expectable).asInstanceOf[MatchResult[S]]
  }
}
