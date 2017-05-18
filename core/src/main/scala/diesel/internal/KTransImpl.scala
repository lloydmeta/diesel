package diesel.internal

import scala.collection.immutable._
import scala.meta._

object KTransImpl {

  def expand(defn: Tree): Stat = {
    defn match {
      case SupportedAnnottee(extracted) => {
        val (algebraName, tparams, template) =
          (extracted.tname, extracted.tparams, extracted.template)
        val tparam  = selectOneFunctor(tparams)
        val builder = new KTransformBuilder(algebraName, tparam, template, extracted.ctorCall)
        val meth    = builder.build()
        extracted.appendStat(meth)
      }
      case Term.Block(Seq(SupportedAnnottee(extracted), companion)) => {
        val (algebraName, tparams, template) =
          (extracted.tname, extracted.tparams, extracted.template)
        val tparam  = selectOneFunctor(tparams)
        val builder = new KTransformBuilder(algebraName, tparam, template, extracted.ctorCall)
        val meth    = builder.build()
        Term.Block(
          Seq(
            extracted.appendStat(meth),
            companion
          )
        )
      }
      case _ => abort(
        s"""
           |At the moment, only traits and abstract classes are supported,
           |but you provided:
           |
           |${defn.syntax}""".stripMargin)
    }
  }

  private val currentTraitHandle = Term.Name("curr")
  private val currentTraitPat    = Pat.Var.Term(currentTraitHandle)
  private val natTransArg = Term.Name("natTrans")

  private def selectOneFunctor(tparams: Seq[Type.Param]): Type.Param = tparams match {
    case Seq(tparam) if tparam.tparams.size == 1 => tparam
    case _ =>
      abort(
        s"This annotation only supports types parameterised with one kind that takes one type argument, but you provided $tparams")
  }

  private class KTransformBuilder(algebraType: Type.Name,
                                  tparam: Type.Param,
                                  template: Template,
                                  ctorRefBuilder: Type => Ctor.Call) {

    private val selfRef = template.self
    private val selfRefTerm: Term.Name = {
      if (selfRef.name.value == Name.Anonymous().value) {
        Term.Name("this")
      } else {
        Term.Name(selfRef.name.value)
      }
    }

    private val targetKName      = pickOther(tparam.name.value)
    private val targetKType      = Type.Name(targetKName)
    private val transformTargetK = tparam.copy(name = targetKType)

    private val boundlessTParam           = tparam.copy(cbounds = Nil)
    private val tparamName                = tparam.name.value
    private val tparamAsType              = Type.fresh().copy(tparamName)
    private val templateStatements        = template.stats.toSeq.flatten
    private val templateStatementsWithIdx = templateStatements.zipWithIndex

    private val algebraTargetKConstructor = ctorRefBuilder(targetKType)

    def build(): Defn.Def = {
      // Extract common variables

      val abstractsWithIdx = abstractMembers
      val concretesWithIdx = concreteMembers

      val abstracts = abstractsWithIdx.map(_._1)
      val concretes = concretesWithIdx.map(_._1)

      ensureSoundMembers(abstracts, concretes)

      val forwardedAbstracts = abstracts.flatMap {
        case Decl.Val(mods, pats,Type.Apply(_, declTpeParams)) => {
          val newdeclTpe  = Type.Apply(targetKType, declTpeParams)
          pats.map { pat =>
            val access =q"""$natTransArg.apply($currentTraitHandle.${pat.name})"""
            Defn.Val(mods, Seq(pat), Some(newdeclTpe), access)
          }
        }
        case Decl.Def(mods, name, tparams, paramss, Type.Apply(_, declTpeParams)) => {
          val tparamTypes = tparams.map(tp => Type.Name(tp.name.value))
          val paramNames  = paramss.map(_.map(tp => Term.Name(tp.name.value)))
          val newdeclTpe  = Type.Apply(targetKType, declTpeParams)
          val body =
            if (tparamTypes.nonEmpty)
              q"""$natTransArg.apply($currentTraitHandle.$name[..$tparamTypes](...$paramNames))"""
            else
              q"""$natTransArg($currentTraitHandle.$name(...$paramNames))"""
          Seq(Defn.Def(mods, name, tparams, paramss, Some(newdeclTpe), body))
        }
      }

      val transformKMeth =
        q"""final def transformK[$transformTargetK]($natTransArg: _root_.diesel.LiteFunK[$tparamAsType, $targetKType]): $algebraType[$targetKType] = {
            val $currentTraitPat = $selfRefTerm
            new $algebraTargetKConstructor {
              ..$forwardedAbstracts
            }
          }"""
      transformKMeth
    }

    private def ensureSoundMembers(dslMembers: List[Stat], concreteMembers: List[Stat]): Unit = {
      val dslMembersSet = dslMembers.toSet
      val concreteMembersSet = concreteMembers.toSet
      // The spaces in multiline strings are significant
      val statsWithErrors = findErrors(
        Seq(
          ("Please use only package private modifiers.", privateMembersPf(concreteMembersSet)),
          ("Return types must be explicitly stated.", noReturnTypePf(concreteMembersSet)),
          ("Abstract type members are not supported", abstractType),
          (s"""The return type of this method is not wrapped in $tparamName[_]. Methods like this can be
              |      added to the trait's companion object.""".stripMargin,
           nonMatchingKindPf(dslMembersSet ++ concreteMembersSet)),
          ("Vars are not allowed.", varsPf(Set.empty)),
          ("Vals that are not assignments are not allowed at the moment",
           patternMatchingVals(concreteMembersSet)),
          (s"""This method has a type parameter that shadows the $tparamName[_] used to annotate the trait.
              |      Besides being confusing for readers of your code, this is not currently supported by diesel.""".stripMargin,
           methodsShadowingTParamPF)
        )
      )

      val specificErrors = statsWithErrors.map {
        case (stat, errors) =>
          val combinedErrors = errors
            .map { e =>
              s"    * $e"
            }
            .mkString("\n")
          s"""  ${stat.syntax}
             |
           |$combinedErrors
         """.stripMargin
      }

      val erroneousStats = statsWithErrors.map(_._1).toSet
      val genUnsupportedStats = templateStatements.filterNot { s =>
        erroneousStats.contains(s) || dslMembersSet.contains(s) || concreteMembersSet.contains(s)
      }
      val genUnsupportedErrs = {
        if (genUnsupportedStats.nonEmpty) {
          val statsStrs = genUnsupportedStats
            .map { stat =>
              s"  * ${stat.syntax}"
            }
            .mkString("\n\n")
          Seq(
            s"""The following are not supported inside the body of a trait annotated with @diesel. If possible, please consider
               |moving them into a companion object:
               |
               |$statsStrs""".stripMargin)
        } else Nil
      }
      val combinedErrMsgs = specificErrors ++ genUnsupportedErrs
      if (combinedErrMsgs.nonEmpty) {
        val errsMsg = combinedErrMsgs.mkString("\n\n")

        abort(s"""
                 |
                 |Looks like you're using some unsupported syntax in a trait annotated with @diesel.
                 |
                 |$errsMsg""".stripMargin)
      }
    }

    private def concreteMembers: List[(Defn, Int)] =
      templateStatementsWithIdx.collect {
        case (v: Defn, i) => (v, i)
      }.toList

    private def abstractMembers: List[(Decl, Int)] =
      templateStatementsWithIdx.collect {
        case (d @ Decl.Def(_, _, _, _, Type.Apply(retName: Type.Select, _)), i)
            if retName.name.value == tparamName =>
          (d, i)
        case (v @ Decl.Val(_, _, Type.Apply(retName: Type.Select, _)), i)
            if retName.name.value == tparamName =>
          (v, i)

        case (d @ Decl.Def(_, _, _, _, Type.Apply(retName: Type.Name, _)), i)
            if retName.value == tparamName =>
          (d, i)
        case (v @ Decl.Val(_, _, Type.Apply(retName: Type.Name, _)), i)
            if retName.value == tparamName =>
          (v, i)
      }.toList

    private def findErrors(
        msgsToPfs: Seq[(String, PartialFunction[Stat, Stat])]): Seq[(Stat, Seq[String])] = {
      templateStatements.foldLeft(Seq.empty[(Stat, Seq[String])]) {
        case (acc, stat) =>
          val errs = msgsToPfs.foldLeft(Seq.empty[String]) {
            case (innerAcc, (str, pf)) =>
              if (pf.isDefinedAt(stat))
                innerAcc :+ str
              else
                innerAcc
          }
          if (errs.nonEmpty) acc :+ (stat -> errs) else acc
      }
    }

    type StatPF = PartialFunction[Stat, Stat]
    private def isPrivate(m: Mod): Boolean = {
      // This is the only reliable way to compare mods...
      m match {
        case mod"private" => true
        case _            => false
      }
    }

    private def privateMembersPf(exempt: Set[Stat]): StatPF = {
      case d @ Defn.Def(mods, _, _, _, _, _) if mods.exists(isPrivate) && !exempt.contains(d) =>
        d
      case v @ Defn.Val(mods, _, _, _) if mods.exists(isPrivate) && !exempt.contains(v) =>
        v
      case d @ Decl.Def(mods, _, _, _, _) if mods.exists(isPrivate) && !exempt.contains(d) =>
        d
      case v @ Decl.Val(mods, _, _) if mods.exists(isPrivate) && !exempt.contains(v) =>
        v
    }

    private def noReturnTypePf(exempt: Set[Stat]): StatPF = {
      case d @ Defn.Def(_, _, _, _, None, _) if !exempt.contains(d) =>
        d
      case v @ Defn.Val(_, _, None, _) if !exempt.contains(v) =>
        v
    }

    private def abstractType: StatPF = {
      case t: Decl.Type => t
    }

    private def nonMatchingKindPf(exempt: Set[Stat]): StatPF = {
      case d: Decl.Def if !exempt.contains(d) => d
      case v: Decl.Val if !exempt.contains(v) => v
    }

    private def varsPf(exempt: Set[Stat]): StatPF = {
      case v: Defn.Var if !exempt.contains(v) => v
    }

    private val methodsShadowingTParamPF: StatPF = {
      case d @ Decl.Def(_, _, tparams, _, _)
          if tparams.exists(tp => tp.name.value == tparamName) =>
        d
    }

    @SuppressWarnings(Array("org.wartremover.warts.IsInstanceOf"))
    private def patternMatchingVals(exempt: Set[Stat]): StatPF = {
      case v @ Defn.Val(_, Seq(first, _ @_ *), _, _)
          if !(exempt.contains(v) || first.isInstanceOf[Pat.Var.Term]) =>
        v
    }

  }

  private val tparamNames: Seq[Char] = ('G' to 'Z') ++ ('A' to 'F')

  private def pickOther(k: String): String =
    tparamNames.collectFirst { case c if k != s"$c" => s"$c" }.getOrElse("Z")

}
