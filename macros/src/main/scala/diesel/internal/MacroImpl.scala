package diesel.internal

import scala.meta._
import scala.collection.immutable._
import scala.meta.Type.Param

object MacroImpl {

  private val Algebra = t"Algebra"

  def expand(defn: Tree): Stat = {
    defn match {
      case q"..$mods trait $tname[..$tparams] extends $template" => {
        tparams match {
          case Seq(tparam) if tparam.tparams.size == 1 => {
            val typedContext = new TypedContext(tparam, template)
            import typedContext._

            // Extract common variables
            val abstractMembers = getAbstractMembers(template)
            if (abstractMembers.isEmpty) {
              abort(s"Did not find any abstract members with $tparam return type. Add some.")
            }
            val concreteMembers = getConcreteMembers(template)
            if (concreteMembers.nonEmpty) {
              abort("Concrete members are not supported at the moment")
            }

            val dslWrappers = generateDslWrappers(abstractMembers)

            q"""..$mods object ${Term.Name(tname.value)}{
                import scala.language.higherKinds
                import _root_.diesel.Dsl

                trait $Algebra[$tparam] {
                  ..$abstractMembers
                }

                ..$dslWrappers
               }
             """
          }
          case _ =>
            abort("Sorry, we only work with one type parameter with one hole")
        }
      }
      case _ => abort("Sorry, we only work on traits")
    }
  }

  private class TypedContext(tparam: Type.Param, template: Template) {

    private val tparamName = tparam.name.value

    def ensureSoundMembers(): Unit = {
      for {
        s  <- template.stats
        st <- s
      } {
        st match {
          case d @ Defn.Def(mods, _, _, _, _, _)
              if mods.exists(e =>
                e == Mod.Private(Name.Anonymous()) || e == Mod.Protected(Name.Anonymous())) => {
            abort(s"Please use only package private or protected: ${d.syntax}")
          }
          case v @ Defn.Val(mods, _, _, _)
              if mods.exists(e =>
                e == Mod.Private(Name.Anonymous()) || e == Mod.Protected(Name.Anonymous())) => {
            abort(s"Please use only package private or protected: ${v.syntax}")
          }
          case d @ Decl.Def(mods, _, _, _, _)
              if mods.exists(e =>
                e == Mod.Private(Name.Anonymous()) || e == Mod.Protected(Name.Anonymous())) => {
            abort(s"Please use only package private or protected: ${d.syntax}")
          }
          case v @ Decl.Val(mods, _, _)
              if mods.exists(e =>
                e == Mod.Private(Name.Anonymous()) || e == Mod.Protected(Name.Anonymous())) => {
            abort(s"Please use only package private or protected: ${v.syntax}")
          }

          case v @ Defn.Def(_, _, _, _, None, _) =>
            abort(s"Return type must be explicitly stated for $v")
          case v @ Defn.Val(_, _, None, _) =>
            abort(s"Return type must be explicitly stated for $v")

          // <-- For IntelliJ
          case d @ Decl.Def(_, _, _, _, retType: Type.Select)
              if retType.name.value != tparam.name.value =>
            abort(
              s"Abstract def needs to have return type ${tparam.name}[...], otherwise, make it non-abstract: ${System
                .lineSeparator()} ${d.syntax}")
          case v @ Decl.Val(_, _, retType: Type.Select)
              if retType.name.value != tparam.name.value =>
            abort(
              s"Abstract val needs to have return type ${tparam.name}[...], otherwise, make it non-abstract: ${System
                .lineSeparator()} ${v.syntax}")
          // For IntelliJ -->

          case d @ Decl.Def(_, _, _, _, retType: Type.Name)
              if retType.value != tparam.name.value =>
            abort(
              s"Abstract def needs to have return type ${tparam.name}[...], otherwise, make it non-abstract: ${System
                .lineSeparator()} ${d.syntax}")
          case v @ Decl.Val(_, _, retType: Type.Name) if retType.value != tparam.name.value =>
            abort(
              s"Abstract val needs to have return type ${tparam.name}[...], otherwise, make it non-abstract: ${System
                .lineSeparator()} ${v.syntax}")
          case v: Defn.Var => abort(s"Found a var, which is not allowed: $v")
          case _           => ()
        }
      }
    }

    def getConcreteMembers(template: Template): List[Defn] = {
      template.stats
        .map { stats =>
          stats.collect {
            case d: Defn.Def => d
            case v: Defn.Val => v
          }.toList
        }
        .getOrElse(Nil)
    }

    def getAbstractMembers(template: Template): List[Decl] = {
      template.stats
        .map { stats =>
          stats.collect {
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
        }
        .getOrElse(Nil)
    }

    def generateDslWrappers(decl: List[Decl]) = {
      def buildWrappedDef(mods: Seq[Mod],
                          name: Term.Name,
                          paramss: Seq[Seq[Term.Param]],
                          tparams: Seq[Param],
                          declTargs: Seq[Type]): Defn.Def = {
        val newParamss = paramss.map { params =>
          params.map { param =>
            param.decltpe match {
              case Some(t"$tParamAsType[..$realParam]") => {
                val decltpe = Some(t"Dsl[$Algebra, ..$realParam]")
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
        val newDeclTpe = t"Dsl[$Algebra, ..$declTargs]"
        val body =
          q"""new Dsl[$Algebra, ..$declTargs] {
               def apply[F[_]](implicit I: $Algebra[F]): F[..$declTargs] = I.$name(...$interpreterArgs)
              }"""
        Defn.Def(mods, name, tparams, newParamss, Some(newDeclTpe), body)
      }

      def buildWrappedVal(mods: Seq[Mod], pats: Seq[Pat.Var.Term], declTargs: Seq[Type]) = {
        val patName = pats match {
          case Seq(p) => p
          case _ =>
            abort(s"""Pattern-matched values are not supported at the moment: $pats""")
        }
        val body       = q"""new Dsl[$Algebra, ..$declTargs] {
               def apply[F[_]](implicit I: $Algebra[F]): F[..$declTargs] = I.${patName.name}
              }"""
        val newDeclTpe = t"Dsl[$Algebra, ..$declTargs]"
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

}
