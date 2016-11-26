import macroni.CompileSpec
import scala.reflect.runtime.universe._

class CodeSpec extends CompileSpec {
  "accept no argument" >> {
    q"""@derive.derived object A""" must compile
  }

  "accept string argument" >> {
    q"""@derive.derived("hi") object A""" must compile
  }

  "accept multiple arguments" >> {
    q"""class A(@derive.derived("hi", "ho") x: Int)""" must compile
  }

  "abort on wrong argument" >> {
    q"""@derive.derived(1) object A""" must abort
  }
}
