package helpers

import org.specs2.mutable.Specification
import scala.compat.Platform.EOL

trait CodeComparisonSpec extends Specification with ContextMock with CompileSpec {
  sequential

  import Colors._
  import contextMock.universe._

  trait ExpectedCode
  case class With(code: String) extends ExpectedCode
  case class Not(code: String) extends ExpectedCode

  implicit def TreeToWith(t: Tree): With = With(showCode(t))
  implicit def StringToWith(code: String): With = With(code)

  private def errorMessage(source: Tree, generated: String, head: String, body: String) = {
    highlight(comparableWildcards(showCode(source))) +
      EOL + bold(red("--- generates: ---")) + EOL +
      highlight(comparableWildcards(generated)) +
      EOL + bold(red(s"--- $head: ---")) + EOL +
      body +
      EOL + bold(red("----------")) + EOL
  }

  private def compileErrorMessage(source: Tree, generated: String, reason: String) = {
    errorMessage(source, generated, "which doesn't compile", reason)
  }

  private def compareErrorMessage(source: Tree, generated: String, snippet: String, shouldContain: Boolean) = {
    val failMsg = if(shouldContain) "which doesn't contain" else "which contains but shouldn't"
    errorMessage(source, generated, failMsg, highlight(comparableWildcards(snippet)))
  }

  private val wildcardRegex = "_\\$\\d+".r
  private def comparableWildcards(code: String) = wildcardRegex.replaceAllIn(code, "_")
  private def withoutSpaces(code: String) = code.replaceAll("\\s", "")
  private def comparable(code: String) = withoutSpaces(comparableWildcards(code))
  private def containsCode(source: Tree, generated: String, snippets: Seq[ExpectedCode]) = snippets.map {
    case With(snippet) =>
      comparable(generated) must (contain(comparable(snippet))).updateMessage(_ => compareErrorMessage(source, generated, snippet, true))
    case Not(snippet)  =>
      comparable(generated) must (not(contain(comparable(snippet)).updateMessage(_ => compareErrorMessage(source, generated, snippet, false))))
  }

  def generatedContainsCode(source: Tree, snippets: ExpectedCode*) = {
    val compiled = compileCode(showCode(source))
    compiled.error.isEmpty must beTrue.updateMessage(_ => compileErrorMessage(source, compiled.generated, compiled.error.get))
    containsCode(source, compiled.generated, snippets)
  }
}

