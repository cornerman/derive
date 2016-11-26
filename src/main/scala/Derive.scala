package derive

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.{StaticAnnotation, compileTimeOnly}

class DeriveTranslator[C <: Context](val context: C) {
  import context.universe._

  def arguments: Seq[String] = context.prefix.tree match {
    case Apply(_, args) => args map {
      case Literal(Constant(arg: String)) => arg
      case _ => context.abort(context.enclosingPosition, "annotation expects constant string arguments")
    }
    case r => Seq.empty
  }

  def translate(tree: Tree): Tree = {
    val s = showCode(tree)
    q"""println($s)"""
  }
}

object DeriveTranslator {
  def apply(c: Context): DeriveTranslator[c.type] = new DeriveTranslator(c)
}

object Derive {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val t = DeriveTranslator(c)
    val args = t.arguments
    val trees = annottees.map(_.tree).map(t.translate)
    c.Expr[Any](q"""Map("options" -> Seq(..$args), "annottees" -> Seq(..$trees))""")
  }
}

@compileTimeOnly("only for compile time expansion")
class derived extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Derive.impl
}
