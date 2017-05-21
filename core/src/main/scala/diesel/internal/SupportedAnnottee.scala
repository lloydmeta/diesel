package diesel.internal

import scala.meta._
import scala.collection.immutable._

trait SupportedAnnottee {

  def mods: Seq[Mod]

  def tname: Type.Name

  def tparams: Seq[Type.Param]

  def template: Template

  def underlying: Stat

  def appendStat(stat: Stat): Stat

  def ctorCall(tpe: Type): Ctor.Call

  def ctorParams: Seq[Seq[Term.Param]]

}

case class TraitAnnottee(mods: Seq[Mod],
                         tname: Type.Name,
                         tparams: Seq[Type.Param],
                         template: Template,
                         underlying: Stat)
    extends SupportedAnnottee {

  def appendStat(stat: Stat): Stat = {
    val newTempl =
      template.copy(stats = template.stats.map(s => s :+ stat).orElse(Some(Seq(stat))))
    q"..$mods trait $tname[..$tparams] extends $newTempl"
  }

  def ctorCall(tpe: Type): Ctor.Call = {
    Term.ApplyType(Ctor.Ref.Name(tname.value), Seq(tpe))
  }

  def ctorParams: Seq[Seq[Term.Param]] = Nil
}

case class ClassAnnottee(mods: Seq[Mod],
                         tname: Type.Name,
                         tparams: Seq[Type.Param],
                         ctor: Ctor.Primary,
                         template: Template,
                         underlying: Stat)
    extends SupportedAnnottee {

  def appendStat(stat: Stat): Stat = {
    val newTempl =
      template.copy(stats = template.stats.map(s => s :+ stat).orElse(Some(Seq(stat))))
    Defn.Class(mods, tname, tparams, ctor, newTempl)
  }

  def ctorCall(tpe: Type): Ctor.Call = {
    val ctorRef = Ctor.Ref.Name(tname.value)
    val args    = ctor.paramss.map(_.map(p => Term.Name(p.name.value)))
    ctor"$ctorRef[$tpe](...$args)"
  }

  def ctorParams: Seq[Seq[Term.Param]] = ctor.paramss
}

object SupportedAnnottee {

  def unapply(tree: Tree): Option[SupportedAnnottee] = tree match {
    case stat @ Defn.Trait(mods, tname, tparams, _, template) =>
      Some(TraitAnnottee(mods, tname, tparams, template, stat))
    case stat @ Defn.Class(mods, tname, tparams, ctor, template) =>
      Some(ClassAnnottee(mods, tname, tparams, ctor, template, stat))
    case _ => None
  }

}
