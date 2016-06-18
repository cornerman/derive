package helpers

import scala.tools.cmd.CommandLineParser
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.{CompilerCommand, Global, Settings}
import scala.compat.Platform.EOL

object Config {
  val paradiseJar = System.getProperty("user.home") + "/.ivy2/cache/org.scalamacros/paradise_2.11.8/jars/paradise_2.11.8-2.1.0.jar"
  val options = s"-Xplugin-require:macroparadise -Xplugin:$paradiseJar"
  val args = CommandLineParser.tokenize(options)
  def emptySettings = new Settings(error => sys.error(s"compilation has failed: $error"))
}

case class CompiledCode(generated: String, error: Option[String])
object CompiledCode {
  def success(generated: String) = CompiledCode(generated, None)
  def failure(generated: String, error: String) = CompiledCode(generated, Some(error))
}

trait CompileSpec {
  def compileCode(code: String): CompiledCode = {
    import Config._

    // Step 1: create and initialize the compiler
    val reporter = new StoreReporter()
    val command = new CompilerCommand(args, emptySettings)
    command.settings.embeddedDefaults[CompileSpec]
    val global = new Global(command.settings, reporter)
    val run = new global.Run
    global.phase = run.parserPhase
    global.globalPhase = run.parserPhase
    import global._

    def errorMessage = {
      s"${global.phase} has failed" +
      EOL + reporter.infos.map(info => s"[${info.severity}] line ${info.pos.line}: ${info.msg}").mkString(EOL)
    }

    // Step 2: parse the input code
    val unit = newCompilationUnit(code, "<test>")
    val tree = newUnitParser(unit).parse()
    if(reporter.hasErrors || reporter.hasWarnings)
      return CompiledCode.failure(showCode(tree), errorMessage)

    // Step 3: typecheck the input code
    import analyzer._
    phase = run.namerPhase
    globalPhase = run.namerPhase
    val namer = newNamer(rootContext(unit))
    namer.enterSym(tree)
    phase = run.typerPhase
    globalPhase = run.typerPhase
    val typer = newTyper(rootContext(unit))
    val typedTree = typer.typed(tree)
    for(workItem <- unit.toCheck) workItem()
    if(reporter.hasErrors || reporter.hasWarnings)
      return CompiledCode.failure(showCode(typedTree), errorMessage)

    CompiledCode.success(showCode(typedTree))
  }
}
