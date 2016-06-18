package derive

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.{StaticAnnotation, compileTimeOnly}

class ContextHelper[C <: Context](val context: C) {
  import context.universe._

  case class ObjectPattern(name: TermName, parents: List[Tree], body: List[Tree])

  object ObjectPattern {
    def unapply(tree: Tree): Option[ObjectPattern] = tree match {
      case q"object $name extends ..$parents { ..$body }" => Some(ObjectPattern(name, parents, body))
      case _ => None
    }
  }

  def translate(tree: Tree): Tree = tree match {
    case ObjectPattern(pattern) => translate(pattern)
    case _ => context.abort(context.enclosingPosition, "Cannot match object pattern")
  }

  def translate(pattern: ObjectPattern): Tree = {
    import pattern._

    q"""
      object $name extends ..$parents {
        def hello: ${typeOf[String]} = "hello"
        ..$body
      }
    """
  }
}

object ContextHelper {
  def apply(c: Context): ContextHelper[c.type] = new ContextHelper(c)
}

object helloMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val h = ContextHelper(c)
    val trees = annottees.map(_.tree).toList
    val translated = h.translate(trees.head) :: trees.tail
    c.Expr[Any](q"..$translated")
  }
}

@compileTimeOnly("only for compile time expansion")
class hello extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro helloMacro.impl
}
