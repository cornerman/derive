import org.scalatest._
import derive.derive

class CodeSpec extends FreeSpec with MustMatchers {
  //TODO macroni with scala meta?

  "implement abstract method definition" in {
    trait CopyT {
      val a: Int
      def copy(a: Int): CopyT
    }

    @derive(a => copy, equals)
    class CopyC(val a: Int, val b: Int) extends CopyT

    val copy: CopyT = new CopyC(1, 2)

    copy.copy(a = 3) mustEqual new CopyC(3, 2)
  }

  "derive case" in {
    @derive(Case)
    class Caese(val a: Int, val b: Long, val c: Double, val d: Float, val e: String)
    val caese = Caese(1, 2, 3.0, 4.0f, "5")

    caese.toString mustEqual s"Caese(${1},${2},${3.0},${4.0f},${5})"
    caese mustEqual Caese(1, 2, 3.0, 4.0f, "5")
    //TODO...
  }

  "derive toString no values" in {
    @derive(toString)
    class Pete

    val pete = new Pete
    pete.toString mustEqual "Pete"
  }

  "derive toString one values" in {
    @derive(foo => toString)
    class Pete(val foo: Int)

    val pete = new Pete(1)
    pete.toString mustEqual "Pete(1)"
  }

  "derive toString on values" in {
    @derive((x,y) => toString)
    trait Tret {
      val x: Int
      val y: Int = 3
      val z: String = "bla"
    }

    val tret = new Tret { val x = 13 }

    tret.toString mustEqual "Tret(13,3)"
  }

  "derive copyF" - {
    @derive((foo,bar) => copyF)
    class Pete(val foo: Int, val bar: String)
    val pete = new Pete(1, "a")

    "no args" in {
      val eqCopy = pete.copyF()
      eqCopy.foo mustEqual pete.foo
      eqCopy.bar mustEqual pete.bar
    }

    "default arg" in {
      val defCopy = pete.copyF(bar = _ + "a")
      defCopy.foo mustEqual pete.foo
      defCopy.bar mustEqual "aa"
    }

    "all args" in {
      val diffCopy = pete.copyF(foo = _ + 1, bar = _ + "a")
      diffCopy.foo mustEqual 2
      diffCopy.bar mustEqual "aa"
    }
  }

  "derive multiple" in {
    @derive((x,y) => (toString, apply), y => copy, unapply, hashCode, equals)
    class Clars(val x: Int, val y: Int)
    object Clars { def apply(x: Int) = new Clars(x, 1) }

    val clars = Clars(13, 14)
    Clars.unapply(clars) mustEqual Some((13, 14))

    val copied = clars.copy(0)
    clars.hashCode must not(equal(copied.hashCode))
    clars must not(equal(copied))
    clars mustEqual Clars(13, 14)
    clars.toString must not(equal("Clars(13, 14)"))
  }

  "derive multiple meta" in {
    @derive(Equality, Factory, Product)
    class Clazz(val x: Int, val y: Int)

    val clazz = Clazz(13, 14)
    clazz mustEqual Clazz(13, 14)
    clazz.productElement(0) mustEqual 13
    clazz.productElement(1) mustEqual 14
  }
}
