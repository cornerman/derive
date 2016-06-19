package helpers

import scala.reflect.runtime.universe._

trait ExpectedCode { val code: Tree }
case class With(code: Tree) extends ExpectedCode
case class Not(code: Tree) extends ExpectedCode

object TreeComparison {
  val wildcardRegex = "_\\$\\d+".r
  def comparableWildcards(code: String): String = wildcardRegex.replaceAllIn(code, "_")
  def comparableWildcards(code: Tree): String = comparableWildcards(showCode(code))
  def withoutSpaces(code: String): String = code.replaceAll("\\s", "")
  def withoutSpaces(code: Tree): String = withoutSpaces(showCode(code))
  def comparable(code: String): String = withoutSpaces(comparableWildcards(code))
  def comparable(code: Tree): String = comparable(showCode(code))

  def containsSnippet(generated: Tree, snippet: Tree) = comparable(generated).contains(comparable(snippet))
  def equalsSnippet(generated: Tree, snippet: Tree) = comparable(generated) == comparable(snippet)

  def containsExpected(generated: Tree, expected: ExpectedCode) = {
    expected match {
      case With(snippet) => containsSnippet(generated, snippet)
      case Not(snippet)  => !containsSnippet(generated, snippet)
    }
  }

  def unexpectedSnippets(source: Tree, generated: Tree, snippets: Seq[ExpectedCode]): Seq[ExpectedCode] = {
    snippets.filterNot(containsExpected(generated, _))
  }
}
