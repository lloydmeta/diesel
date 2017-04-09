package diesel.internal

import scala.meta._
import scala.collection.immutable._
import scala.meta.Type.Param

object MacroImpl {

  private val DslType = t"_root_.diesel.Dsl"
  private val DslCtor = ctor"_root_.diesel.Dsl"

  def expand(self: Tree, defn: Tree): Stat = {
    val algebraType: Type.Name = {
      val arg = self match {
        case q"new $_(${Lit(arg: String)})" => arg
        case _                              => _root_.diesel.diesel.DefaultAlgebraName
      }
      Type.Name(arg)
    }
    defn match {
      // No companion object
      case q"..$mods trait $tname[..$tparams] extends $template" => {
        val TaglessFinalTrees(statements, dslWrappers) = buildTrees(algebraType, tparams, template)
        q"""..$mods object ${Term.Name(tname.value)} {
             ..${statements.stats}
             ..$dslWrappers
           }
          """
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
        Term.Block(Seq(companion.copy(templ = newTemplate)))
      }
      case _ => abort("Sorry, we only work on traits")
    }
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

    private val tparamName         = tparam.name.value
    private val tparamAsType       = Type.fresh().copy(tparamName)
    private val templateStatements = template.stats.toSeq.flatten

    def build(): TaglessFinalTrees = {
      // Extract common variables
      val abstracts = abstractMembers

      ensureSoundMembers(abstracts)

      val dslWrappers = generateDslWrappers(abstracts)
      val statements  = q"""
                import scala.language.higherKinds

                trait $algebraType[$tparam] {
                  ..$abstracts
                }
        """
      TaglessFinalTrees(statements, dslWrappers)
    }

    private def ensureSoundMembers(abstractMembers: List[Decl]): Unit = {
      def buildErr(intro: String, stats: List[Stat]): Option[String] = {
        if (stats.nonEmpty) {
          val errs = stats
            .map { stat =>
              s"  * ${stat.syntax}"
            }
            .mkString("\n\n")
          Some(s"""$intro:
               |
               |$errs""".stripMargin)
        } else None
      }

      val privsErrMsg    = buildErr("Please use only package private or protected", privateMembers)
      val lackReturnsMsg = buildErr("Return type must be explicitly stated", noReturnType)
      val wrongKindsMsg  = buildErr("Return type must be explicitly stated", nonMatchingKind)
      val mutsMsg        = buildErr("Found one or more vars, which are not allowed", vars)
      val concreteMembersMsg = buildErr(
        """Currently, only abstract defs and vals are supported inside the body of a trait annotated with @diesel.
          |If you wish to write concrete members, please add them to a companion object (the trait will be expanded into
          |the object)""".stripMargin,
        concreteMembers
      )

      val combinedErrs: Seq[String] =
        Seq(privsErrMsg, lackReturnsMsg, wrongKindsMsg, mutsMsg, concreteMembersMsg).flatten
      if (combinedErrs.nonEmpty) {
        val errsMsg = combinedErrs.mkString("\n\n")

        abort(s"""
             |
             |Looks like you're using some unsupported syntax in a trait annotated with @diesel.
             |
             |$errsMsg
           """.stripMargin)
      }
    }

    private def privateMembers =
      templateStatements.collect {
        case d @ Defn.Def(mods, _, _, _, _, _)
            if mods.exists(
              e => e == Mod.Private(Name.Anonymous()) || e == Mod.Protected(Name.Anonymous())) => {
          d
        }
        case v @ Defn.Val(mods, _, _, _)
            if mods.exists(
              e => e == Mod.Private(Name.Anonymous()) || e == Mod.Protected(Name.Anonymous())) => {
          v
        }
        case d @ Decl.Def(mods, _, _, _, _)
            if mods.exists(
              e => e == Mod.Private(Name.Anonymous()) || e == Mod.Protected(Name.Anonymous())) => {
          d
        }
        case v @ Decl.Val(mods, _, _)
            if mods.exists(
              e => e == Mod.Private(Name.Anonymous()) || e == Mod.Protected(Name.Anonymous())) => {
          v
        }
      }.toList

    private def noReturnType =
      templateStatements.collect {
        case d @ Defn.Def(_, _, _, _, None, _) =>
          d
        case v @ Defn.Val(_, _, None, _) =>
          v
      }.toList

    private def nonMatchingKind =
      templateStatements.collect {
        // <-- For IntelliJ
        case d @ Decl.Def(_, _, _, _, retType: Type.Select)
            if retType.name.value != tparam.name.value =>
          d
        case v @ Decl.Val(_, _, retType: Type.Select) if retType.name.value != tparam.name.value =>
          v
        // For IntelliJ -->
        case d @ Decl.Def(_, _, _, _, retType: Type.Name) if retType.value != tparam.name.value =>
          d
        case v @ Decl.Val(_, _, retType: Type.Name) if retType.value != tparam.name.value => v
      }.toList

    private def vars =
      templateStatements.collect {
        case v: Defn.Var => v
      }.toList

    private def concreteMembers =
      templateStatements.collect {
        case d: Defn.Def => d
        case v: Defn.Val => v
      }.toList

    private def abstractMembers: List[Decl] =
      templateStatements.collect {
        case m @ Decl.Def(_, _, _, _, Type.Apply(retName: Type.Select, _))
            if retName.name.value == tparamName =>
          m
        case v @ Decl.Val(_, _, Type.Apply(retName: Type.Select, _))
            if retName.name.value == tparamName =>
          v

        case m @ Decl.Def(_, _, _, _, Type.Apply(retName: Type.Name, _))
            if retName.value == tparamName =>
          m
        case v @ Decl.Val(_, _, Type.Apply(retName: Type.Name, _))
            if retName.value == tparamName =>
          v
      }.toList

    private def generateDslWrappers(decl: List[Decl]): List[Defn] = {
      def buildWrappedDef(mods: Seq[Mod],
                          name: Term.Name,
                          paramss: Seq[Seq[Term.Param]],
                          tparams: Seq[Param],
                          declTargs: Seq[Type]): Defn.Def = {
        val newParamss = paramss.map { params =>
          params.map { param =>
            param.decltpe match {
              case Some(t"$tParamAsType[..$realParam]") => {
                val decltpe = Some(t"$DslType[$algebraType, ..$realParam]")
                param.copy(decltpe = decltpe)
              }
              case _ => param
            }
          }
        }
        val interpreterArgs: Seq[Seq[Term.Arg]] = paramss.map { params =>
          params.map { param =>
            param.decltpe match {
              case Some(t"$tParamAsType[..$realParam]") => {
                val term = Term.Name(param.name.value)
                q"$term.apply[$tParamAsType]"
              }
              case _ => {
                Term.Name(param.name.value)
              }
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

      decl.map {
        case Decl.Def(mods, name, tparams, paramss, Type.Apply(retName: Type.Select, declTargs))
            if retName.name.value == tparamName =>
          buildWrappedDef(mods, name, paramss, tparams, declTargs)
        case Decl.Val(mods,
                      pats @ Seq(patName, _ @_ *),
                      Type.Apply(retName: Type.Select, declTargs))
            if retName.name.value == tparamName =>
          buildWrappedVal(mods, pats, declTargs)

        case Decl.Def(mods, name, tparams, paramss, Type.Apply(retName: Type.Name, declTargs))
            if retName.value == tparamName =>
          buildWrappedDef(mods, name, paramss, tparams, declTargs)
        case v @ Decl.Val(mods, pats, Type.Apply(retName: Type.Name, declTargs))
            if retName.value == tparamName =>
          buildWrappedVal(mods, pats, declTargs)
      }
    }

  }

  private case class TaglessFinalTrees(modules: Term.Block, wrapperMethods: List[Defn])

}
