package diesel.internal

import scala.meta._
import scala.collection.immutable._

trait SupportedAnnottee {

  def mods: Seq[Mod]

  def tname: Type.Name

  def tparams: Seq[Type.Param]

  def template: Template

  def underlying: Stat

}

case class TraitAnnottee(mods: Seq[Mod],
                         tname: Type.Name,
                         tparams: Seq[Type.Param],
                         template: Template)
    extends SupportedAnnottee {

  def underlying: Stat = {
    q"..$mods trait $tname[..$tparams] extends $template"
  }
}

case class ClassAnnottee(mods: Seq[Mod],
                         tname: Type.Name,
                         tparams: Seq[Type.Param],
                         ctor: Ctor.Primary,
                         template: Template)
    extends SupportedAnnottee {
  def underlying: Stat = {
    Defn.Class(mods, tname, tparams, ctor, template)
  }
}

object SupportedAnnottee {

  def unapply(tree: Tree): Option[SupportedAnnottee] = tree match {
    case q"..$mods trait $tname[..$tparams] extends $template" =>
      Some(TraitAnnottee(mods, tname, tparams, template))
    case Defn.Class(mods, tname, tparams, ctor, template) =>
      Some(ClassAnnottee(mods, tname, tparams, ctor, template))
    case _ => None
  }

}
