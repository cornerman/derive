import helpers.CompileSpec
import scala.reflect.runtime.universe._

class CodeSpec extends CompileSpec {

  "simple hello generates" >> {
    q"""@derive.hello object A""" must compile.to(
      q"""object A { def hello: String = "hello" }"""
    )
  }

  "simple hello generates containing" >> {
    q"""@derive.hello object A""" must compile.containing(
      q""""hello""""
    )
  }

  "simple hello compiles" >> {
    q"@derive.hello object A" must compile
    q"@derive.hello object A" must not(abort)
    q"@derive.hello object A" must compile.canWarn
    q"@derive.hello object A" must compile.containing(q"def hello: String")
  }

  "duplicate hello doesn't compile" >> {
    q"@derive.hello object A { val hello = 1 }" must abort
    q"@derive.hello object A { val hello = 1 }" must not(compile)
    q"@derive.hello object A { val hello = 1 }" must abort(startWith("reflective typecheck has failed: method hello is defined twice")).canWarn
    q"@derive.hello object A { val hello = 1 }" must compile.withError
  }

  "detect warning" >> {
    q"@derive.hello object A { def bar[T](l: T) = 1.isInstanceOf[T] }" must warn
    q"@derive.hello object A { def bar[T](l: T) = 1.isInstanceOf[T] }" must compile.canWarn
    q"@derive.hello object A { def bar[T](l: T) = 1.isInstanceOf[T] }" must warn("abstract type T is unchecked since it is eliminated by erasure")
    q"@derive.hello object A { def bar[T](l: T) = 1.isInstanceOf[T] }" must warn(contain("erasure"))
    q"@derive.hello object A { def bar[T](l: T) = 1.isInstanceOf[T] }" must compile.withWarning
    q"@derive.hello object A { def bar[T](l: T) = 1.isInstanceOf[T] }" must compile.withWarning.containing(q"object A")
  }

  "detect error" >> {
    q"@derive.hello object A { val foo: String = 2 }" must abort
    q"@derive.hello object A { val foo: String = 2 }" must abort("reflective typecheck has failed: type mismatch;\n found   : Int(2)\n required: String")
    q"@derive.hello object A { val foo: String = 2 }" must compile.withError
  }

  "detect abort in macro" >> {
    q"@derive.hello class A" must abort
    q"@derive.hello class A" must abort("reflective typecheck has failed: Cannot match object pattern")
    q"@derive.hello class A" must compile.withError
  }
}
