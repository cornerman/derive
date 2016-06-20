import helpers.CompileSpec
import scala.reflect.runtime.universe._

class CodeSpec extends CompileSpec {

  "simple hello generates" >> {
    q"""@derive.hello object A""".compile must compile.to(
      q"""object A { def hello: String = "hello" }"""
    )
  }

  "simple hello generates containing" >> {
    q"""@derive.hello object A""".compile must compile.containing(
      q""""hello""""
    )
  }

  "simple hello compiles" >> {
    q"@derive.hello object A".compile must compile
    q"@derive.hello object A".compile must compile.canWarn
    q"@derive.hello object A".compile must compile.containing(q"")
  }

  "duplicate hello doesn't compile" >> {
    q"@derive.hello object A { val hello = 1 }".compile must abort
  }

  "detect warning" >> {
    q"@derive.hello object A { def bar[T](l: T) = 1.isInstanceOf[T] }".compile must warn
    q"@derive.hello object A { def bar[T](l: T) = 1.isInstanceOf[T] }".compile must compile.canWarn
    q"@derive.hello object A { def bar[T](l: T) = 1.isInstanceOf[T] }".compile must warn("abstract type T is unchecked since it is eliminated by erasure")
    q"@derive.hello object A { def bar[T](l: T) = 1.isInstanceOf[T] }".compile must compile.withWarning
    q"@derive.hello object A { def bar[T](l: T) = 1.isInstanceOf[T] }".compile must compile.withWarning.containing(q"")
  }

  "detect error" >> {
    q"@derive.hello object A { val foo: String = 2 }".compile must abort
    q"@derive.hello object A { val foo: String = 2 }".compile must abort("reflective typecheck has failed: type mismatch;\n found   : Int(2)\n required: String")
    q"@derive.hello object A { val foo: String = 2 }".compile must compile.withError
  }

  "detect error" >> {
    q"@derive.hello class A".compile must abort
    q"@derive.hello class A".compile must abort("reflective typecheck has failed: Cannot match object pattern")
    q"@derive.hello class A".compile must compile.withError
  }
}
