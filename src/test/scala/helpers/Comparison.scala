package helpers

trait ExpectedCode { val code: String }
case class With(code: String) extends ExpectedCode
case class Not(code: String) extends ExpectedCode

object TreeComparison {
  val wildcardRegex = "_\\$\\d+".r
  def comparableWildcards(code: String) = wildcardRegex.replaceAllIn(code, "_")
  def withoutSpaces(code: String) = code.replaceAll("\\s", "")
  def comparable(code: String) = withoutSpaces(comparableWildcards(code))
}

object ErrorMessage {
  import Colors._, TreeComparison._
  import scala.compat.Platform.EOL
  import scala.reflect.runtime.universe._

  def error(source: Tree, generated: Tree, head: String, body: String) = {
    highlight(comparableWildcards(showCode(source))) +
      EOL + bold(red("--- generates: ---")) + EOL +
      highlight(comparableWildcards(showCode(generated))) +
      EOL + bold(red(s"--- $head: ---")) + EOL +
      body +
      EOL + bold(red("----------")) + EOL
  }

  def compileError(source: Tree, generated: Tree, reason: String) = {
    error(source, generated, "which doesn't compile", reason)
  }

  def compareError(source: Tree, generated: Tree, expected: ExpectedCode) = {
    val failMsg = expected match {
      case With(_) => "which doesn't contain"
      case Not(_) => "which contains but shouldn't"
    }
    error(source, generated, failMsg, highlight(comparableWildcards(expected.code)))
  }
}

