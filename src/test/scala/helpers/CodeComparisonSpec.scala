package helpers

import org.specs2.mutable.Specification
import org.specs2.matcher.{Matcher, MatchResult, Expectable}
import scala.reflect.runtime.universe._

trait CodeComparisonSpec extends Specification with CompileSpec {
  sequential

  import ErrorMessage._, TreeComparison._

  implicit def TreeToWith(t: Tree): With = With(showCode(t))
  implicit def StringToWith(code: String): With = With(code)

  def containsSnippet(generated: Tree, snippet: String) = comparable(showCode(generated)).contains(comparable(snippet))

  def containsExpected(generated: Tree, expected: ExpectedCode) = {
    expected match {
      case With(snippet) => containsSnippet(generated, snippet)
      case Not(snippet)  => !containsSnippet(generated, snippet)
    }
  }

  def unexpectedSnippets(source: Tree, generated: Tree, snippets: Seq[ExpectedCode]): Seq[ExpectedCode] = {
    snippets.filterNot(containsExpected(generated, _))
  }

  def compiledContains(snippets: Seq[ExpectedCode]) = new Matcher[Tree] {
    import scala.compat.Platform.EOL

    override def apply[T <: Tree](expectable: Expectable[T]): MatchResult[T] = {
      val source = expectable.value
      compileCode(source) match {
        case CompileError(generated, error) =>
          result(false, "", compileError(source, generated, error), expectable)
        case CompiledCode(generated) =>
          val failures = unexpectedSnippets(source, generated, snippets)
          val failMsg = failures.map(failure => compareError(source, generated, failure)).mkString(EOL)
          result(failures.isEmpty, "Everything as expected", failMsg, expectable)
      }
    }
  }
}

