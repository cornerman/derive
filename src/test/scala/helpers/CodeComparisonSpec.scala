package helpers

import compiler._
import org.specs2.mutable.Specification
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

trait CodeComparisonSpec extends Specification with CompileSpec {
  import ErrorMessage._, TreeComparison._

  implicit def TreeToWith(t: Tree): With = With(t)
  implicit def StringToWith(code: String): With = With(q"$code")

  object expand {
    def to(tree: Tree) = new EqualsMatcher(tree)
    def containing(snippets: ExpectedCode*) = new ContainsMatcher(snippets)
  }
}
