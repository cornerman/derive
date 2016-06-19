package compiler

import scala.tools.reflect.FrontEnd
import scala.reflect.runtime.universe.Tree
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
import scala.util.{Try, Success, Failure}

class Reporter extends FrontEnd {
  override def display(info: Info) {}
  override def interactive() {}
}

object Config {
  val paradiseJar = System.getProperty("user.home") + "/.ivy2/cache/org.scalamacros/paradise_2.11.8/jars/paradise_2.11.8-2.1.0.jar"
  val options = s"-Xplugin-require:macroparadise -Xplugin:$paradiseJar"
}

object Compiler {
  def apply(tree: Tree): CompileResult = {
    val reporter = new Reporter
    val toolbox = currentMirror.mkToolBox(reporter, Config.options)

    Try(toolbox.typecheck(tree)) match {
      case Success(typedTree) => CompileSuccess(tree, typedTree, reporter)
      case Failure(e) => CompileFailure(tree, e)
    }
  }
}
