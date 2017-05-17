package diesel.internal

import scala.meta._
import scala.collection.immutable._
import _root_.diesel.Defaults

object MacroImpl {

  def expand(self: Tree, defn: Tree): Stat = {
    val opsName: Term.Name = {
      val arg = self match {
        case q"new $_(${Lit.String(s)})" => s
        case _                           => Defaults.OpsName
      }
      Term.Name(arg)
    }
    defn match {
      // No companion object
      case SupportedAnnottee(extracted) => {
        val (mods, tname, tparams, template) =
          (extracted.mods, extracted.tname, extracted.tparams, extracted.template)
        val TaglessFinalTrees(algebraAlias, singletonAlias, applyMethod, opsObject) =
          buildTrees(tname, opsName, tparams, template)
        Term.Block(
          Seq(
            extracted.underlying,
            q"""..${objectModsOnly(mods)} object ${Term.Name(tname.value)} {
               $algebraAlias
               $singletonAlias
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
        val TaglessFinalTrees(algebraAlias, singletonAlias, applyMethod, opsObject) =
          buildTrees(tname, opsName, tparams, template)
        val templateStats: Seq[Stat] =
          Seq(algebraAlias, singletonAlias, applyMethod, opsObject) ++ companion.templ.stats
            .getOrElse(Nil)
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
      case Seq(tparam) => {
        val typedContext = new TaglessFinalBuilder(algebraName, opsName, tparam, template)
        typedContext.build()
      }
      case _ =>
        abort(s"""
            |Sorry, we only work with one type parameter per algebra, but you passed:
            |
            |  ${tparams.mkString(", ")}
            |""".stripMargin)
    }
  }

  // Constants that we reuse when aliasing types
  private val algebraAliasName   = Type.Name("AlgebraTypeAlias")
  private val singletonAliasName = Type.Name("SingletonTypeAlias")
  private val singletonTypeAlias = q"private type $singletonAliasName = this.type"

  private class TaglessFinalBuilder(algebraName: Type.Name,
                                    opsObjectName: Term.Name,
                                    tparam: Type.Param,
                                    template: Template) {

    private val tparamName      = tparam.name.value
    private val tparamAsType    = Type.fresh().copy(tparamName)
    private val boundlessTparam = tparam.copy(cbounds = Nil)

    private val interpreterType = Type.Apply(algebraName, Seq(tparamAsType))

    // Aliasing our types in case the user decides to get creative with more types/objects
    // inside their companion
    private val algebraTypeAlias          = q"private type $algebraAliasName[$boundlessTparam] = $interpreterType"
    private val aliasedInterpreterType    = Type.Apply(algebraAliasName, Seq(tparamAsType))
    private val aliasedAlgebraBoundTParam = tparam.copy(cbounds = Seq(algebraAliasName))

    private val singletonToInterpMethName =
      Term.Name(s"singleton${algebraName.value}To${opsObjectName.value}")

    def build(): TaglessFinalTrees = {

      val applyMethod =
        q"def apply[$aliasedAlgebraBoundTParam]: $aliasedInterpreterType = ${implicitlyTree(aliasedInterpreterType)}"

      val toOpsMethod =
        q"implicit def $singletonToInterpMethName[$aliasedAlgebraBoundTParam](o: $singletonAliasName): $aliasedInterpreterType = ${implicitlyTree(aliasedInterpreterType)}"

      val opsWrapper =
        q"""object $opsObjectName {
           $toOpsMethod
           }"""
      TaglessFinalTrees(algebraTypeAlias, singletonTypeAlias, applyMethod, opsWrapper)
    }

    private def implicitlyTree(t: Type): Term.ApplyType = q"_root_.scala.Predef.implicitly[$t]"
  }

  private final case class TaglessFinalTrees(algebraTypeAlias: Defn.Type,
                                             singletonTypeAlias: Defn.Type,
                                             applyMethod: Defn.Def,
                                             opsObject: Defn.Object)

}
