import org.specs2.mutable.Specification
import derive.derive

class CodeSpec extends Specification {
  //TODO macroni with scala meta?

  "implement abstract method definition" >> {
    trait CopyT {
      val a: Int
      def copy(a: Int): CopyT
    }

    @derive(copy)
    case class CopyC(a: Int, b: Int) extends CopyT

    val copy = CopyC(1, 2)

    copy.copy(b = 3) must beEqualTo(CopyC(1, 3))
  }

  "derive case" >> {
    @derive(Case)
    class Caese(val a: Int, val b: Long, val c: Double, val d: Float, val e: String)
    val caese = Caese(1, 2, 3.0, 4.0f, "5")

    caese.toString must beEqualTo("""Caese(1,2,3.0,4.0,5)""") //TODO
    caese must beEqualTo(Caese(1, 2, 3.0, 4.0f, "5"))
    //TODO...
  }

  "derive toString on values" >> {
    @derive((x,y) => toString)
    trait Tret {
      val x: Int
      val y: Int = 3
      val z: String = "bla"
    }

    val tret = new Tret { val x = 13 }

    tret.toString must beEqualTo("Tret(13,3)")
  }

  "derive multiple" >> {
    @derive((x,y) => (toString, apply), y => copy, unapply, hashCode, equals)
    class Clars(val x: Int, val y: Int)
    object Clars { def apply(x: Int) = new Clars(x, 1) }

    val clars = Clars(13, 14)
    clars match {
      case Clars(13, 14) => ok
      case _ => failure
    }

    val copied = clars.copy(0)
    clars.hashCode must not(beEqualTo(copied.hashCode))
    clars must not(beEqualTo(copied))
    clars must beEqualTo(Clars(13, 14))
    clars.toString must not(beEqualTo("Clars(13, 14)"))
  }

  "derive multiple meta" >> {
    @derive(Equality, Factory, Product)
    class Clazz(val x: Int, val y: Int)

    val clazz = Clazz(13, 14)
    clazz must beEqualTo(Clazz(13, 14))
    clazz.productElement(0) must beEqualTo(13)
    clazz.productElement(1) must beEqualTo(14)
  }
}


object MyApp extends App {
}
