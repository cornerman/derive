import helpers.CodeComparisonSpec
import scala.reflect.runtime.universe._

class CodeSpec extends CodeComparisonSpec {

  "simple hello" >> {
    q"""@derive.hello object A""" must compiledContains(Seq(
      q"""object A { def hello: String = "hello" }"""
    ))
  }

  "simple hello compiles" >> {
    q"""@derive.hello object A""" must compile
  }

  "duplicate hello doesn't compile" >> {
    q"""@derive.hello object A { val hello = 1 } """ must not(compile)
  }
}
