package derive

import scala.meta._
import scala.collection.immutable.Seq
import scala.reflect.ClassTag

object EitherHelper {
  def sequence[A, B](s: Seq[Either[A, B]]): Either[A, Seq[B]] = s collectFirst { case Left(l) => Left(l) } getOrElse Right(s collect { case Right(x) => x })
}
import EitherHelper._

sealed trait ModuleDef {
  val name: String
  val templ: Template
  val defn: Defn
  val companion: Option[Defn.Object]
  def toTree = companion.map(c => q"$defn; $c").getOrElse(defn)
  def copy(templ: Template = templ, companion: Option[Defn.Object] = companion): ModuleDef
}

case class ClassDef(defn: Defn.Class, companion: Option[Defn.Object]) extends ModuleDef {
  val name = defn.name.value
  val templ = defn.templ
  def copy(templ: Template, companion: Option[Defn.Object]) = ClassDef(defn.copy(templ = templ), companion)
}

case class TraitDef(defn: Defn.Trait, companion: Option[Defn.Object]) extends ModuleDef {
  val name = defn.name.value
  val templ = defn.templ
  def copy(templ: Template, companion: Option[Defn.Object]) = TraitDef(defn.copy(templ = templ), companion)
}

case class Value(name: Term.Name, tpe: Type.Arg)

object ExtractValues {
  def fromTempl(templ: Template) = templ.stats.toSeq.flatten.collect {
    case v: Decl.Val => v.pats.map(pat => Value(pat.name, v.decltpe))
    case v: Defn.Val => v.pats.collect { case p: Pat.Var.Term => Value(p.name, v.decltpe.get) }
  }.flatten

  def fromCtor(ctor: Ctor.Primary) = ctor.paramss.flatten.collect {
    case Term.Param(_, Term.Name(name), Some(tpe), _) => Value(Term.Name(name), tpe)
  }

  def fromDefn(defn: Defn) = defn match {
    case c: Defn.Class => fromTempl(c.templ) ++ fromCtor(c.ctor)
    case t: Defn.Trait => fromTempl(t.templ)
    case _ => Seq.empty
  }
}

sealed trait ValueSelection { def select(module: ModuleDef): Either[String, Seq[Value]] }
case object AutoValues extends ValueSelection {
  def select(module: ModuleDef) = module match {
    case ClassDef(c, _) => Right(ExtractValues.fromCtor(c.ctor))
    case m => Left(s"cannot select arguments from constructor: type '${m.name}' is not a class")
  }
}
case class ValuesByName(names: Seq[String]) extends ValueSelection {
  def select(module: ModuleDef) = {
    val definedVals = ExtractValues.fromDefn(module.defn)
    sequence(names.map(name => definedVals.find(_.name.value == name) match {
      case Some(v) => Right(v)
      case None => Left(s"missing value definition for member '$name' in type '${module.name}'")
    }))
  }
}

sealed trait GenMethod
case class InstanceMethod(method: Defn.Def) extends GenMethod
case class CompanionMethod(method: Defn.Def) extends GenMethod

case class PatchConfig(name: String, selection: ValueSelection)

sealed trait Patch
sealed trait Parent extends Patch { def ctor: Ctor.Call }
sealed trait Method extends Patch { def apply(module: ModuleDef): Either[String, GenMethod] }

object PatchDsl {
  type Generator[T] = T => Either[String, GenMethod]
  implicit def ModuleDefToRight(t: GenMethod): Either[String, GenMethod] = Right(t)

  sealed trait PatchContainer {
    def patches: Set[PatchContainer]
    def patch: Option[Patch]
    def +(p: PatchContainer): PatchGroup = PatchGroup(patches ++ Set(p))

    def resolve(seen: Set[PatchContainer] = Set.empty): Set[Patch] = {
      if (seen contains this) seen.flatMap(_.patch)
      else {
        val newSeen = seen ++ Set(this)
        patches.map(_.resolve(newSeen)).fold(Set.empty)(_ ++ _)
      }
    }
  }

  case class PatchGroup(patches: Set[PatchContainer]) extends PatchContainer {
    def patch = None
  }
  object PatchGroup { def apply(patches: PatchContainer*) = new PatchGroup(patches.toSet) }

  abstract class MapPatch(depends: => PatchContainer) extends PatchContainer with Patch {
    final def patch = Some(this)
    final def patches = Set(this) ++ depends.patches
  }

  abstract class MapParent(name: String, depends: => PatchContainer = PatchGroup()) extends MapPatch(depends) with Parent {
    def ctor = Ctor.Name(name)
  }

  abstract class MapMethod(f: Generator[ModuleDef], depends: => PatchContainer = PatchGroup()) extends MapPatch(depends) with Method {
    def apply(module: ModuleDef) = f(module)
  }

  def select[T <: ModuleDef](selection: ValueSelection)(f: Generator[(T, Seq[Value])]): Generator[T] = t => selection.select(t).right.flatMap(values => f((t, values)))

  def on[T <: ModuleDef : ClassTag](f: Generator[T]): Generator[ModuleDef] = t => t match {
    case o: T => f(o)
    case _ => Left(s"cannot generate method ???: type '${t.name}' is not a ${implicitly[ClassTag[T]].runtimeClass.getSimpleName}")
  }
}

object Patch {
  import PatchDsl._

  //TODO: fresh variables everywhere
  case class Copy(selection: ValueSelection) extends MapMethod(on[ClassDef](select(selection) { case (c, values) =>
    val params = values.map(v => Term.Param(List.empty, v.name, Some(v.tpe), Some(v.name))).toList
    val names = c.defn.ctor.paramss.map(_.map(v => Term.Name(v.name.value))).toList
    val ctor = Ctor.Name(c.defn.name.value)
    InstanceMethod(q"def copy(..$params) = new $ctor(...$names)")
  }))

  case class Apply(selection: ValueSelection) extends MapMethod(on[ClassDef](select(selection) { case (c, values) =>
    //TODO: missing args?
    val params = values.map(v => Term.Param(List.empty, v.name, Some(v.tpe), None)).toList
    val names = c.defn.ctor.paramss.map(_.map(v => Term.Name(v.name.value))).toList
    val ctor = Ctor.Name(c.defn.name.value)
    CompanionMethod(q"def apply(..$params) = new $ctor(...$names)")
  }))

  case class ToString(selection: ValueSelection) extends MapMethod(select(selection) { case (m, values) =>
    val names = values.map(_.name).toList
    InstanceMethod(q"override def toString: String = ${m.name} + (..$names)") //TODO tuple warning
  })

  case class Unapply(selection: ValueSelection) extends MapMethod(select(selection) { case (m, values) =>
    //TODO: missing vals?
    val vals = values.map(v => q"t.${v.name}").toList
    val res = if (values.size == 1) vals.head else q"(..$vals)"
    val tpe = Type.Name(m.name)
    CompanionMethod(q"def unapply(that: $tpe) = Option(that).map(t => $res)")
  })

  case class HashCode(selection: ValueSelection) extends MapMethod(select(selection) { case (m, values) =>
    val accs = values.map {
      case Value(name, t"Int") => q"$name"
      case Value(name, t"Long") => q"scala.runtime.Statics.longHash($name)"
      case Value(name, t"Double") => q"scala.runtime.Statics.doubleHash($name)"
      case Value(name, t"Float") => q"scala.runtime.Statics.floatHash($name)"
      case Value(name, _) => q"scala.runtime.Statics.anyHash($name)"
    }.map(mix => q"acc = scala.runtime.Statics.mix(acc, $mix)")

    InstanceMethod(q"""override def hashCode: Int = {
      var acc: Int = -889275714
      ..$accs
      scala.runtime.Statics.finalizeHash(acc, ${accs.size})
    }""")
  })

  case class Equals(selection: ValueSelection) extends MapMethod(select(selection) { case (m, values) =>
    val tpe = Type.Name(m.name)
    val comparisons = values.map(v => q"(t.${v.name} == $tpe.this.${v.name})")
    val condition = comparisons.fold(q"$tpe.this.canEqual(that)")((a,b) => q"$a && $b")

    InstanceMethod(q"""override def equals(that: Any) = $tpe.this.eq(that.asInstanceOf[Object]) || (that match {
      case (t: $tpe) => $condition
      case _ => false
    })""")
  }, depends = CanEqual)

  case class ProductElement(selection: ValueSelection) extends MapMethod(select(selection) { case (m, values) =>
    val tpe = Type.Name(m.name)
    val cases = values.zipWithIndex.map { case (v,i) => p"case $i => $tpe.this.${v.name}" }

    InstanceMethod(q"""def productElement(n: Int): Any = n match {
      ..case $cases
      case _ => throw new IndexOutOfBoundsException(n.toString())
    }""")
  })

  case class ProductArity(selection: ValueSelection) extends MapMethod(select(selection) { case (m, values) =>
    InstanceMethod(q"def productArity: Int = ${values.size}")
  })

  case class ProductIterator(selection: ValueSelection) extends MapMethod(m => {
    val tpe = Type.Name(m.name)
    InstanceMethod(q"override def productIterator: Iterator[Any] = scala.runtime.ScalaRunTime.typedProductIterator[Any]($tpe.this)")
  }, depends = Product(selection))

  case object ProductPrefix extends MapMethod(m => {
    InstanceMethod(q"override def productPrefix: String = ${m.name}")
  }) //TODO depends on parent whether override needed or not

  case object CanEqual extends MapMethod(m => {
    val tpe = Type.Name(m.name)
    InstanceMethod(q"def canEqual(that: Any) = $tpe.this.isInstanceOf[$tpe]")
  })

  case class Product(selection: ValueSelection) extends MapParent("Product", depends = ProductArity(selection) + ProductElement(selection) + ProductPrefix + ProductIterator(selection) + CanEqual)

  def Equality(selection: ValueSelection) = Equals(selection) + HashCode(selection)
  def Factory(selection: ValueSelection) = Apply(selection) + Unapply(selection)
  def Case(selection: ValueSelection) = ToString(selection) + Copy(selection) + Equality(selection) + Product(selection) + Factory(selection)

  def apply(c: PatchConfig): Either[String, Set[Patch]] = {
    val container = Some(c.name).collect {
      case "canEqual" => CanEqual //TODO c.selection: Left(s"cannot create canEqual on value selection: $v"}
      case "productPrefix" => ProductPrefix //TODO c.selection: Left(s"cannot create productPrefix on value selection: $v"}
      case "productIterator" => ProductIterator(c.selection)
      case "productArity" => ProductArity(c.selection)
      case "productElement" => ProductElement(c.selection)
      case "toString" => ToString(c.selection)
      case "unapply" => Unapply(c.selection)
      case "hashCode" => HashCode(c.selection)
      case "equals" => Equals(c.selection)
      case "copy" => Copy(c.selection)
      case "apply" => Apply(c.selection)
      case "Product" => Product(c.selection)
      case "Equality" => Equality(c.selection)
      case "Factory" => Factory(c.selection)
      case "Case" => Case(c.selection)
    }

    container.map(_.resolve()).toRight(s"unknown patch: ${c.name}")
  }
}

object Derive {
  def updateTemplate(templ: Template, methods: Seq[Defn.Def], parents: Seq[Ctor.Call]): Template = {
    //TOOD: check for existing...
    val newStats = templ.stats.toSeq.flatten ++ methods
    templ.copy(stats = Some(Seq(newStats: _*)), parents = parents)
  }

  def updateCompanion(name: String, companion: Option[Defn.Object], methods: Seq[Defn.Def], parents: Seq[Ctor.Call]): Option[Defn.Object] = {
    companion.map { comp =>
      Some(comp.copy(templ = updateTemplate(comp.templ, methods, parents)))
    } getOrElse {
      if (methods.isEmpty) None else Some(q"object ${Term.Name(name)} extends ..$parents { ..$methods }")
    }
  }

  def parseConfigs(configs: Seq[PatchConfig]): Either[String, Seq[Patch]] = sequence(configs.map(Patch(_))).right.map(_.flatten)

  def deriveModule(patches: Seq[Patch])(module: ModuleDef): Either[String, ModuleDef] = {
    val distinctPatches = patches.distinct
    val methods = distinctPatches.collect { case m: Method => m }
    val parents = distinctPatches collect { case p: Parent => p.ctor }
    sequence(methods.map(_(module))).right.map { generatedMethods =>
      val instMethods = generatedMethods collect { case InstanceMethod(m) => m }
      val compMethods = generatedMethods collect { case CompanionMethod(m) => m }
      val newTempl = updateTemplate(module.templ, instMethods, parents)
      val newComp = updateCompanion(module.name, module.companion, compMethods, Seq.empty)
      module.copy(newTempl, newComp)
    }
  }
}

class derive extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    import Derive._

    val Term.New(Template(_, Seq(Term.Apply(_, args)), _, _)) = this
    val configs = args.map {
      case Term.Function(params, names) => (names match {
        case Term.Name(name) => List(name)
        case Term.Tuple(vals) => vals map {
          case Term.Name(name) => name
          case arg => abort(s"unexpected argument in tuple: $arg")
        }
      }, ValuesByName(params.map(_.toString)))
      case Term.Name(name) => (List(name), AutoValues)
      case arg => abort(s"unexpected argument: $arg")
    } flatMap { case (ms, sel) => ms.map(m => PatchConfig(m, sel)) }

    val deriver = parseConfigs(configs).right.map(deriveModule(_) _)

    val module = defn match {
      case t: Defn.Trait => TraitDef(t, None)
      case c: Defn.Class => ClassDef(c, None)
      case d => d.children match {
        case (t: Defn.Trait) :: (o: Defn.Object) :: Nil => TraitDef(t, Some(o))
        case (t: Defn.Class) :: (o: Defn.Object) :: Nil => ClassDef(t, Some(o))
        case _ => abort(s"unexpected annotation: $defn")
      }
    }

    deriver.right.flatMap(_(module)) match {
      case Left(err) => abort(err)
      case Right(derived) => derived.toTree
    }
  }
}

//TODO: case class:
//  - class/trait toString depend on Product? scala.runtime.ScalaRunTime._toString(Foo.this);
//  - companion with apply implements AbstractFunctionN[in..., Type], Serializable
//  - companion has toString = typename
