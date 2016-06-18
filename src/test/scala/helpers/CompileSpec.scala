package helpers

import org.specs2.mutable.Specification
import org.specs2.matcher.{Matcher, MatchResult, Expectable}
import scala.tools.reflect.FrontEnd
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.Tree
import scala.tools.reflect.ToolBox
import scala.compat.Platform.EOL
import scala.util.{Try, Success, Failure}

object Config {
  val paradiseJar = System.getProperty("user.home") + "/.ivy2/cache/org.scalamacros/paradise_2.11.8/jars/paradise_2.11.8-2.1.0.jar"
  val options = s"-Xplugin-require:macroparadise -Xplugin:$paradiseJar"
}

sealed trait CompileResult { val generated: Tree }
case class CompiledCode(generated: Tree) extends CompileResult
case class CompileError(generated: Tree, error: String) extends CompileResult

class Reporter extends FrontEnd {
  override def display(info: Info) {}
  override def interactive() {}

  def errorMessage: String = {
    infos.map(info => s"[${info.severity}] line ${info.pos.line}: ${info.msg}").mkString(EOL)
  }
}

trait CompileSpec extends Specification {
  import ErrorMessage._

  def compileCode(tree: Tree): CompileResult = {
    val reporter = new Reporter
    val toolbox = currentMirror.mkToolBox(reporter, Config.options)

    Try(toolbox.typecheck(tree)) match {
      case Success(typedTree) => 
        if (reporter.hasErrors || reporter.hasWarnings)
          CompileError(typedTree, reporter.errorMessage)
        else
          CompiledCode(typedTree)
      case Failure(e) => CompileError(tree, e.getMessage)
    }
  }

  def compile = new Matcher[Tree] {
    override def apply[T <: Tree](expectable: Expectable[T]): MatchResult[T] = {
      val source = expectable.value
      compileCode(source) match {
        case CompileError(generated, error) =>
          result(false, "", compileError(source, generated, error), expectable)
        case CompiledCode(generated) =>
          result(true, "Compiles", "", expectable)
      }
    }
  }
}
