package diesel.internal

import scala.meta._
import scala.collection.immutable._
import _root_.diesel.Defaults

object MacroImpl {

  def expand(self: Tree, defn: Tree): Stat = {
    val opsName: Term.Name = {
      val arg = self match {
        case q"new $_(${Lit.String(s)})" => s
        case _                           => Defaults.DslName
      }
      Term.Name(arg)
    }
    defn match {
      // No companion object
      case SupportedAnnottee(extracted) => {
        val (mods, tname, tparams, template) =
          (extracted.mods, extracted.tname, extracted.tparams, extracted.template)
        val TaglessFinalTrees(applyMethod, opsObject) =
          buildTrees(tname, opsName, tparams, template)
        Term.Block(
          Seq(
            extracted.underlying,
            q"""..${objectModsOnly(mods)} object ${Term.Name(tname.value)} {
               $applyMethod
               $opsObject
           }
          """
          ))

      }
      // There is a companion object
      case Term.Block(
          Seq(
            SupportedAnnottee(extracted),
            companion: Defn.Object
          )
          ) => {
        val (mods, tname, tparams, template) =
          (extracted.mods, extracted.tname, extracted.tparams, extracted.template)
        val TaglessFinalTrees(applyMethod, opsObject) =
          buildTrees(tname, opsName, tparams, template)
        val templateStats: Seq[Stat] =
          applyMethod +:opsObject +: companion.templ.stats.getOrElse(Nil)
        val newObjTemplate = companion.templ.copy(stats = Some(templateStats))
        Term.Block(
          Seq(
            extracted.underlying,
            companion.copy(templ = newObjTemplate)
          )
        )
      }
      case other =>
        abort(s"""
             |Sorry, the @diesel annotation currently only works on traits and classes, but you passed:
             |
             |  ${other.syntax}
             |""".stripMargin)
    }
  }

  private def objectModsOnly(ms: Seq[Mod]): Seq[Mod] = ms.filter {
    case mod"final"    => false
    case mod"abstract" => false
    case mod"sealed"   => false
    case mod"override" => false
    case mod"lazy"     => false
    case _             => true
  }

  private def buildTrees(algebraName: Type.Name,
                         opsName: Term.Name,
                         tparams: Seq[Type.Param],
                         template: Template): TaglessFinalTrees = {
    tparams match {
      case Seq(tparam) if tparam.tparams.size == 1 => {
        val typedContext = new TaglessFinalBuilder(algebraName, opsName, tparam, template)
        typedContext.build()
      }
      case _ =>
        abort(s"""
            |Sorry, we only work with one type parameter with one hole, but you passed:
            |
            |  ${tparams.mkString(", ")}
            |""".stripMargin)
    }
  }

  private class TaglessFinalBuilder(algebraName: Type.Name,
                                    opsObjectName: Term.Name,
                                    tparam: Type.Param,
                                    template: Template) {

    val algebraBoundTParam = tparam.copy(cbounds = Seq(algebraName)) // We drop all existing CBounds
    val tparamName = tparam.name.value

    val tparamAsType = Type.fresh().copy(tparamName)
    val interpreterType = Type.Apply(algebraName, Seq(tparamAsType))

    val singletonToInterpMethName = Term.Name(s"singletonTo${opsObjectName.value}")
    val singletonType = Type.Name(s"${algebraName.value}.type")

    def build(): TaglessFinalTrees = {

      val applyMethod = q"def apply[$algebraBoundTParam]: $interpreterType = ${implicitlyTree(interpreterType)}"

      val toOpsMethod = q"implicit def $singletonToInterpMethName[$algebraBoundTParam](o: $singletonType): $interpreterType = ${implicitlyTree(interpreterType)}"


      val opsWrapper =
        q"""object $opsObjectName {
           $toOpsMethod
           }"""
      TaglessFinalTrees(applyMethod, opsWrapper)
    }

    def implicitlyTree(t: Type): Term.ApplyType =  q"_root_.scala.Predef.implicitly[$t]"
  }

  private case class TaglessFinalTrees(applyMethod: Defn.Def, opsObject: Defn.Object)

}
