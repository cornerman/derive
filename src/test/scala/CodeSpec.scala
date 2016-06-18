import helpers.CodeComparisonSpec

class CodeSpec extends CodeComparisonSpec {

  import contextMock.universe._

  "simple hello" >> {
    generatedContainsCode(
      q"""@derive.hello object A""",
      q"""object A {
        def hello: String = "hello";
      }"""
    )
  }
}
