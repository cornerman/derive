package helpers

import scala.tools.reflect.FrontEnd
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
import scala.compat.Platform.EOL

object Config {
  val paradiseJar = System.getProperty("user.home") + "/.ivy2/cache/org.scalamacros/paradise_2.11.8/jars/paradise_2.11.8-2.1.0.jar"
  val options = s"-Xplugin-require:macroparadise -Xplugin:$paradiseJar"
}

case class CompiledCode(generated: String, error: Option[String])
object CompiledCode {
  def success(generated: String) = CompiledCode(generated, None)
  def failure(generated: String, error: String) = CompiledCode(generated, Some(error))
}

class Reporter extends FrontEnd {
  override def display(info: Info) {}
  override def interactive() {}

  def errorMessage: String = {
    infos.map(info => s"[${info.severity}] line ${info.pos.line}: ${info.msg}").mkString(EOL)
  }
}

trait CompileSpec {
  def compileCode(code: String): CompiledCode = {
    val reporter = new Reporter
    val toolbox = currentMirror.mkToolBox(reporter, Config.options)
    import toolbox.u._

    val tree = toolbox.parse(code)
    if (reporter.hasErrors || reporter.hasWarnings)
      return CompiledCode.failure(showCode(tree), reporter.errorMessage)

    val typedTree = toolbox.typecheck(tree)
    if (reporter.hasErrors || reporter.hasWarnings)
      return CompiledCode.failure(showCode(typedTree), reporter.errorMessage)

    CompiledCode.success(showCode(typedTree))
  }
}
