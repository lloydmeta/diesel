package diesel.internal

import scala.meta._
import scala.collection.immutable._
import scala.meta.Type.Param
import _root_.diesel.Defaults

object MacroImpl {

  private val DslType = t"_root_.diesel.Dsl"
  private val DslCtor = ctor"_root_.diesel.Dsl"

  def expand(self: Tree, defn: Tree): Stat = {
    val algebraType: Type.Name = {
      val arg = self match {
        case q"new $_(${Lit(arg: String)})" => arg
        case _                              => Defaults.AlgebraName
      }
      Type.Name(arg)
    }
    defn match {
      // No companion object
      case q"..$mods trait $tname[..$tparams] extends $template" => {
        val TaglessFinalTrees(statements, dslWrappers) = buildTrees(algebraType, tparams, template)
        Term.Block(
          Seq(
            // Emitted empty private trait for IntelliJ
            q"private sealed trait $tname",
            q"""..${objectModsOnly(mods)} object ${Term.Name(tname.value)} {
             ..${statements.stats}
             ..$dslWrappers

           }
          """
          ))

      }
      // There is a companion object
      case Term.Block(
          Seq(
            q"..$mods trait $tname[..$tparams] extends $template",
            companion: Defn.Object
          )
          ) => {
        val TaglessFinalTrees(statements, dslWrappers) = buildTrees(algebraType, tparams, template)
        val templateStats: Seq[Stat] =
          statements.stats ++ dslWrappers ++ companion.templ.stats.getOrElse(Nil)
        val newTemplate = companion.templ.copy(stats = Some(templateStats))
        Term.Block(
          Seq(
            // Emitted empty private trait for IntelliJ
            q"private sealed trait $tname",
            companion.copy(templ = newTemplate)
          )
        )
      }
      case _ => abort("Sorry, we only work on traits")
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

  private def buildTrees(algebraType: Type.Name,
                         tparams: Seq[Type.Param],
                         template: Template): TaglessFinalTrees = {
    tparams match {
      case Seq(tparam) if tparam.tparams.size == 1 => {
        val typedContext = new TaglessFinalBuilder(algebraType, tparam, template)
        typedContext.build()
      }
      case _ =>
        abort("Sorry, we only work with one type parameter with one hole")
    }
  }

  private class TaglessFinalBuilder(algebraType: Type.Name, tparam: Type.Param, template: Template) {

    private val tparamName                = tparam.name.value
    private val tparamAsType              = Type.fresh().copy(tparamName)
    private val templateStatements        = template.stats.toSeq.flatten
    private val templateStatementsWithIdx = templateStatements.zipWithIndex

    def build(): TaglessFinalTrees = {
      // Extract common variables

      val importsWithIdx   = importStats
      val abstractsWithIdx = abstractMembers
      val concretesWithIdx = concreteMembers
      val localsWithIdx    = localMembers

      val imports   = importsWithIdx.map(_._1)
      val abstracts = abstractsWithIdx.map(_._1)
      val concretes = concretesWithIdx.map(_._1)
      val locals    = localsWithIdx.map(_._1)

      ensureSoundMembers(abstracts ++ concretes, locals, imports)

      // Sometimes the ordering matters (e.g. when there are vals involved), so slot in
      // the supported statements in the order the user declared them
      val sortedTemplateStats = {
        val ss = importsWithIdx ++ abstractsWithIdx ++ concretesWithIdx ++ localsWithIdx
        ss.sortBy(_._2).map(_._1)
      }
      val traitTemplate =
        template"""{..${template.early}} with ..${template.parents} { ${template.self} =>
                    ..$sortedTemplateStats
                   }"""
      val statements  = q"""
                import scala.language.higherKinds

                trait $algebraType[$tparam] extends $traitTemplate
        """
      val dslWrappers = generateDslWrappers(abstracts, concretes)
      TaglessFinalTrees(statements, dslWrappers)
    }

    private def ensureSoundMembers(dslMembers: List[Stat],
                                   locals: List[Stat],
                                   imports: List[Import]): Unit = {
      val dslMembersSet = dslMembers.toSet
      val localsSet     = locals.toSet
      // The spaces in multiline strings are significant
      val statsWithErrors = findErrors(
        Seq(
          ("Please use only package private or protected modifiers.", privateMembersPf(localsSet)),
          ("Return types must be explicitly stated.", noReturnTypePf(localsSet)),
          (s"""The return type of this method is not wrapped in $tparamName[_]. Methods like this can be
             |      added to the trait's companion object.""".stripMargin,
           nonMatchingKindPf(dslMembersSet ++ localsSet)),
          ("Vars are not allowed.", varsPf(localsSet)),
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
        erroneousStats.contains(s) || dslMembersSet.contains(s) || localsSet.contains(s) || imports
          .contains(s)
      }
      val genUnsupportedErrs = {
        if (genUnsupportedStats.nonEmpty) {
          val statsStrs = genUnsupportedStats
            .map { stat =>
              s"  * ${stat.syntax}"
            }
            .mkString("\n\n")
          Seq(
            s"""The following are not supported inside the body of a trait annotated with @diesel. Please consider
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
             |If you want to have some of these moved as-is into the generated algebra, please use the
             |@local annotation. Note that such members will not have DSL-wrapping methods generated
             |for them.
             |
             |$errsMsg
           """.stripMargin)
      }
    }

    private def concreteMembers: List[(Defn, Int)] =
      templateStatementsWithIdx.collect {
        case (d: Defn.Def, i) => (d, i)
        case (v: Defn.Val, i) => (v, i)
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

    private def importStats: List[(Import, Int)] =
      templateStatementsWithIdx.collect {
        case (imp: Import, i) => (imp, i)
      }.toList

    private def isLocal(mod: Mod): Boolean = mod match {
      case mod"@local" | mod"@local()" | mod"@diesel.local" | mod"@diesel.local()" => true
      case _                                                                       => false
    }

    private def localMembers: List[(Stat, Int)] =
      templateStatementsWithIdx.collect {
        case s @ (Decl.Def(mods, _, _, _, _), _) if mods.exists(isLocal) =>
          s
        case s @ (Decl.Val(mods, _, _), _) if mods.exists(isLocal) =>
          s
        case s @ (Decl.Type(mods, _, _, _), _) if mods.exists(isLocal) =>
          s
        case s @ (Decl.Var(mods, _, _), _) if mods.exists(isLocal) =>
          s

        case s @ (Defn.Def(mods, _, _, _, _, _), _) if mods.exists(isLocal) =>
          s
        case s @ (Defn.Val(mods, _, _, _), _) if mods.exists(isLocal) =>
          s
        case s @ (Defn.Type(mods, _, _, _), _) if mods.exists(isLocal) =>
          s
        case s @ (Defn.Var(mods, _, _, _), _) if mods.exists(isLocal) =>
          s
        case s @ (Defn.Macro(mods, _, _, _, _, _), _) if mods.exists(isLocal) =>
          s
        case s @ (Defn.Trait(mods, _, _, _, _), _) if mods.exists(isLocal) =>
          s
        case s @ (Defn.Class(mods, _, _, _, _), _) if mods.exists(isLocal) =>
          s
        case s @ (Defn.Object(mods, _, _), _) if mods.exists(isLocal) =>
          s
        case s @ (q"..$mods def this(...$paramss) = $expr", _) if mods.exists(isLocal) => s
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
    private def isPrivateOrProtected(m: Mod): Boolean = {
      // This is the only reliable way to compare mods...
      m match {
        case mod"protected" | mod"private" => true
        case _                             => false
      }
    }

    private def privateMembersPf(exempt: Set[Stat]): StatPF = {
      case d @ Defn.Def(mods, _, _, _, _, _)
          if mods.exists(isPrivateOrProtected) && !exempt.contains(d) =>
        d
      case v @ Defn.Val(mods, _, _, _)
          if mods.exists(isPrivateOrProtected) && !exempt.contains(v) =>
        v
      case d @ Decl.Def(mods, _, _, _, _)
          if mods.exists(isPrivateOrProtected) && !exempt.contains(d) =>
        d
      case v @ Decl.Val(mods, _, _) if mods.exists(isPrivateOrProtected) && !exempt.contains(v) =>
        v
    }
    private def noReturnTypePf(exempt: Set[Stat]): StatPF = {
      case d @ Defn.Def(_, _, _, _, None, _) if !exempt.contains(d) =>
        d
      case v @ Defn.Val(_, _, None, _) if !exempt.contains(v) =>
        v
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

    private def generateDslWrappers(decls: List[Decl], defns: List[Defn]): List[Defn] = {
      def buildWrappedDef(mods: Seq[Mod],
                          name: Term.Name,
                          paramss: Seq[Seq[Term.Param]],
                          tparams: Seq[Param],
                          declTargs: Seq[Type]): Defn.Def = {
        val newParamss = paramss.map { params =>
          params.map { param =>
            param.decltpe match {
              case Some(Type.Apply(t: Type.Name, realParams)) if t.value == tparamName => {
                val decltpe = Some(t"$DslType[$algebraType, ..$realParams]")
                param.copy(decltpe = decltpe)
              }
              case _ => param
            }
          }
        }
        val interpreterArgs: Seq[Seq[Term.Arg]] = paramss.map { params =>
          params.map { param =>
            param.decltpe match {
              case Some(Type.Apply(t: Type.Name, _)) if t.value == tparamName => {
                val term = Term.Name(param.name.value)
                q"$term.apply[$tparamAsType]"
              }
              case _ => Term.Name(param.name.value)
            }
          }
        }
        val newDeclTpe = t"$DslType[$algebraType, ..$declTargs]"
        val body =
          q"""new $DslCtor[$algebraType, ..$declTargs] {
               def apply[$tparam](implicit I: $algebraType[$tparamAsType]): $tparamAsType[..$declTargs] = I.$name(...$interpreterArgs)
              }"""
        Defn.Def(mods, name, tparams, newParamss, Some(newDeclTpe), body)
      }

      // TODO move abort to ensureSoundness
      def buildWrappedVal(mods: Seq[Mod], pats: Seq[Pat.Var.Term], declTargs: Seq[Type]) = {
        val patName = pats match {
          case Seq(p) => p
          case _ =>
            abort(s"""Pattern-matched values are not supported at the moment: $pats""")
        }
        val body       = q"""new $DslCtor[$algebraType, ..$declTargs] {
               def apply[$tparam](implicit I: $algebraType[$tparamAsType]): $tparamAsType[..$declTargs] = I.${patName.name}
              }"""
        val newDeclTpe = t"$DslType[$algebraType, ..$declTargs]"
        Defn.Val(mods, pats, Some(newDeclTpe), body)
      }

      (decls ++ defns).collect {
        case Decl.Def(mods, name, tparams, paramss, Type.Apply(retName: Type.Select, declTargs))
            if retName.name.value == tparamName =>
          buildWrappedDef(mods, name, paramss, tparams, declTargs)
        case Decl.Val(mods,
                      pats @ Seq(patName, _ @_ *),
                      Type.Apply(retName: Type.Select, declTargs))
            if retName.name.value == tparamName =>
          buildWrappedVal(mods, pats, declTargs)

        case Defn.Def(mods,
                      name,
                      tparams,
                      paramss,
                      Some(Type.Apply(retName: Type.Select, declTargs)),
                      _) if retName.name.value == tparamName =>
          buildWrappedDef(mods, name, paramss, tparams, declTargs)
        case Defn.Val(mods,
                      Seq(patName: Pat.Var.Term),
                      Some(Type.Apply(retName: Type.Select, declTargs)),
                      _) if retName.name.value == tparamName =>
          buildWrappedVal(mods, Seq(patName), declTargs)

        case Decl.Def(mods, name, tparams, paramss, Type.Apply(retName: Type.Name, declTargs))
            if retName.value == tparamName =>
          buildWrappedDef(mods, name, paramss, tparams, declTargs)
        case v @ Decl.Val(mods, pats, Type.Apply(retName: Type.Name, declTargs))
            if retName.value == tparamName =>
          buildWrappedVal(mods, pats, declTargs)

        case Defn.Def(mods,
                      name,
                      tparams,
                      paramss,
                      Some(Type.Apply(retName: Type.Name, declTargs)),
                      _) if retName.value == tparamName =>
          buildWrappedDef(mods, name, paramss, tparams, declTargs)
        case v @ Defn.Val(mods,
                          Seq(patName: Pat.Var.Term, _ @_ *),
                          Some(Type.Apply(retName: Type.Name, declTargs)),
                          _) if retName.value == tparamName =>
          buildWrappedVal(mods, Seq(patName), declTargs)
      }
    }

  }

  private case class TaglessFinalTrees(modules: Term.Block, wrapperMethods: List[Defn])

}
