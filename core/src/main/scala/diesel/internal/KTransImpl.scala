package diesel.internal

import diesel.Defaults

import scala.collection.immutable._
import scala.meta._

object KTransImpl {

  def expand(self: Tree, defn: Tree): Stat = {
    val transKMethodName: Term.Name = {
      val arg = self match {
        case q"new $_(${Lit.String(s)})" => s
        case _                           => Defaults.TransKMethodName
      }
      Term.Name(arg)
    }

    defn match {
      case SupportedAnnottee(extracted) => {
        val (algebraName, tparams, template) =
          (extracted.tname, extracted.tparams, extracted.template)
        val tparam = selectOneFunctor(tparams)
        val builder = new TransformKMethBuilder(transKMethodName = transKMethodName,
                                                algebraType = algebraName,
                                                tparam = tparam,
                                                template = template,
                                                ctorRefBuilder = extracted.ctorCall)
        val meth = builder.build()
        extracted.appendStat(meth)
      }
      case Term.Block(Seq(SupportedAnnottee(extracted), companion)) => {
        val (algebraName, tparams, template) =
          (extracted.tname, extracted.tparams, extracted.template)
        val tparam = selectOneFunctor(tparams)
        val builder = new TransformKMethBuilder(transKMethodName = transKMethodName,
                                                algebraType = algebraName,
                                                tparam = tparam,
                                                template = template,
                                                ctorRefBuilder = extracted.ctorCall)
        val meth = builder.build()
        Term.Block(
          Seq(
            extracted.appendStat(meth),
            companion
          )
        )
      }
      case _ => abort(s"""
           |At the moment, only traits and abstract classes are supported,
           |but you provided:
           |
           |${defn.syntax}""".stripMargin)
    }
  }

  private val transKTypeSuffix   = "TransK"
  private val currentTraitHandle = Term.Name("curr")
  private val currentTraitPat    = Pat.Var.Term(currentTraitHandle)
  private val natTransArg        = Term.Name("natTrans")

  private def pickOther(tpname: Type.Param.Name): Type.Name = {
    val k = tpname.value
    Type.Name(s"$k$transKTypeSuffix")
  }

  private def selectOneFunctor(tparams: Seq[Type.Param]): Type.Param = tparams match {
    case Seq(tparam) if tparam.tparams.size == 1 => tparam
    case _ =>
      abort(
        s"This annotation only supports types parameterised with one kind that takes one type argument, but you provided $tparams")
  }

  private class TransformKMethBuilder(transKMethodName: Term.Name,
                                      algebraType: Type.Name,
                                      tparam: Type.Param,
                                      template: Template,
                                      ctorRefBuilder: Type => Ctor.Call) {

    def build(): Defn.Def = {
      ensureSoundMembers()
      val forwardedAbstracts = abstracts.flatMap {
        case Decl.Val(mods, pats, Type.Apply(_, declTpeParams)) => {
          val newdeclTpe = Type.Apply(targetKType, declTpeParams)
          pats.map { pat =>
            val access = q"""$natTransArg.apply($currentTraitHandle.${pat.name})"""
            Defn.Val(mods, Seq(pat), Some(newdeclTpe), access)
          }
        }
        case origDef: Decl.Def => {
          val defWithTransKedTParams = bumpTypeParamsToTransKed(origDef)
          val (mods, name, tparams, paramss, newDeclType) = (defWithTransKedTParams.mods, defWithTransKedTParams.name, defWithTransKedTParams.tparams, defWithTransKedTParams.paramss, defWithTransKedTParams.decltpe)
          val tparamTypes            = tparams.map(tp => Type.Name(tp.name.value))
          val paramNames             = paramss.map(_.map(tp => Term.Name(tp.name.value)))
          val body =
            if (tparamTypes.nonEmpty)
              q"""$natTransArg.apply($currentTraitHandle.$name[..$tparamTypes](...$paramNames))"""
            else
              q"""$natTransArg($currentTraitHandle.$name(...$paramNames))"""
          Seq(Defn.Def(mods, name, tparams, paramss, Some(newDeclType), body))
        }
      }

      q"""
      final def $transKMethodName[$transformTargetK]($natTransArg: _root_.diesel.FunK[$tparamAsType, $targetKType]): $algebraType[$targetKType] = {
        val $currentTraitPat = $selfRefTerm
        new $algebraTargetKConstructor {
          ..$forwardedAbstracts
        }
      }"""
    }

    private def ensureSoundMembers(): Unit = {
      val dslMembersSet      = (abstracts: List[Stat]).toSet
      val concreteMembersSet = (concretes: List[Stat]).toSet
      // The spaces in multiline strings are significant
      val statsWithErrors = findErrors(
        Seq(
          (s"""Abstract methods with parameters that have types parameterised by the same kind as the annottee
               |     ($tparamName[_]) are not supported""".stripMargin,
           paramsParameterisedByKind),
          ("Please use only package private modifiers.", privateMembersPf(concreteMembersSet)),
          ("Return types must be explicitly stated.", noReturnTypePf(concreteMembersSet)),
          ("Abstract type members are not supported", abstractType),
          (s"""The return type of this method is not wrapped in $tparamName[_]. Methods like this can be
              |    added to the trait's companion object.""".stripMargin,
           nonMatchingKindPf(dslMembersSet ++ concreteMembersSet)),
          ("Vars are not allowed.", varsPf(Set.empty)),
          ("Vals that are not assignments are not allowed at the moment",
           patternMatchingVals(concreteMembersSet)),
          (s"""This method has a type parameter that shadows the $tparamName[_] used to annotate the trait.
              |    Besides being confusing for readers of your code, this is not currently supported by diesel.""".stripMargin,
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
            s"""The following are not supported inside the body of a trait annotated with @ktrans. If possible, please consider
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
                 |Looks like you're using some unsupported syntax in a trait annotated with @ktrans.
                 |
                 |$errsMsg""".stripMargin)
      }
    }

    private val selfRef = template.self
    private val selfRefTerm: Term.Name =
      if (selfRef.name.value == Name.Anonymous().value)
        Term.Name("this") // no name
      else
        Term.Name(selfRef.name.value)

    private val targetKType      = pickOther(tparam.name)
    private val transformTargetK = tparam.copy(name = targetKType)

    private val tparamName         = tparam.name.value
    private val tparamAsType       = Type.fresh().copy(tparamName)
    private val templateStatements = template.stats.toSeq.flatten

    private val algebraTargetKConstructor = ctorRefBuilder(targetKType)

    private val concretes: List[Defn] =
      templateStatements.collect {
        case v: Defn => v
      }.toList

    private val abstracts: List[Decl] =
      templateStatements.collect {
        case d @ Decl.Def(_, _, _, _, Type.Apply(retName: Type.Select, _))
            if retName.name.value == tparamName =>
          d
        case v @ Decl.Val(_, _, Type.Apply(retName: Type.Select, _))
            if retName.name.value == tparamName =>
          v
        case d @ Decl.Def(_, _, _, _, Type.Apply(retName: Type.Name, _))
            if retName.value == tparamName =>
          d
        case v @ Decl.Val(_, _, Type.Apply(retName: Type.Name, _))
            if retName.value == tparamName =>
          v
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

    private def paramsParameterisedByKind: StatPF = {
      case d: Decl.Def if paramssParameterisedByKind(d.paramss) => d
    }

    private def paramssParameterisedByKind(paramss: Seq[Seq[Term.Param]]) = {
      val flattened = paramss.flatten
      flattened.collectFirst {
        case p @ Term.Param(_, _, Some(Type.Apply(Type.Name(s), _)), _) if s == tparamName => p
      }.nonEmpty
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

    private def typeParams(meth: Decl.Def): Set[String] = {
      meth.tparams.map { tp =>
        tp.name.value
      }.toSet
    }

    private def bumpTypeParamsToTransKed(meth: Decl.Def): Decl.Def = {
      val tParamNames = typeParams(meth)
      val newtParams = meth.tparams.map { tparam =>
        bumpTParam(tparam, tParamNames)
      }
      val newDeclTpe = {
        // We depend on ensureSoundness having run by the point this is run; otherwise this could fail
        val Type.Apply(_ , declTpeParams) = meth.decltpe
        Type.Apply(targetKType, declTpeParams.map(t => bumpType(t, tParamNames)))
      }
      val newParamss = meth.paramss.map { params =>
        params.map { param =>
          val bumpedTArg = param.decltpe.map {
            case Type.Arg.Repeated(tpe) => Type.Arg.Repeated(bumpType(tpe, tParamNames))
            case Type.Arg.ByName(tpe)   => Type.Arg.ByName(bumpType(tpe, tParamNames))
            case t: Type => bumpType(t, tParamNames)
          }
          param.copy(decltpe = bumpedTArg)
        }
      }
      meth.copy(tparams = newtParams, paramss = newParamss, decltpe = newDeclTpe)
    }

    private def bumpTParam(tparam: Type.Param, baseTypeParams: Set[String]): Type.Param = {
      val bumpedTparamName: Type.Param.Name =
        if (baseTypeParams.contains(tparam.name.value))
          Type.Name(s"${tparam.name.value}$transKTypeSuffix")
        else
          tparam.name
      val bumpedTParamTParams = tparam.tparams.map(tp => bumpTParam(tp, baseTypeParams))

      val bumpedCBounds = tparam.cbounds.map(cb => bumpType(cb, baseTypeParams))
      val bumpedVBounds = tparam.vbounds.map(vb => bumpType(vb, baseTypeParams))
      val bumpedTBounds = {
        val Type.Bounds(lo, hi) = tparam.tbounds
        Type.Bounds(lo.map(l => bumpType(l, baseTypeParams)),
                    hi.map(h => bumpType(h, baseTypeParams)))
      }
      tparam.copy(name = bumpedTparamName,
                  tparams = bumpedTParamTParams,
                  tbounds = bumpedTBounds,
                  vbounds = bumpedVBounds,
                  cbounds = bumpedCBounds)
    }

    private def bumpType(tpe: Type, typesToBump: Set[String]): Type = tpe match {
      case Type.Name(v) if typesToBump.contains(v) => Type.Name(s"$v$transKTypeSuffix")
      case tSelect @ Type.Select(_, Term.Name(v)) if typesToBump.contains(v) =>
        tSelect.copy(name = Type.Name(s"$v$transKTypeSuffix"))
      case tApply @ Type.Apply(tpeInner, args) =>
        tApply.copy(bumpType(tpeInner, typesToBump), args.map(a => bumpType(a, typesToBump)))
      case tApplyInfix @ Type.ApplyInfix(lhs, Type.Name(op), rhs) => {
        val opBumped =
          if (typesToBump.contains(op))
            Type.Name(s"$op$transKTypeSuffix")
          else
            Type.Name(op)
        tApplyInfix.copy(lhs = bumpType(lhs, typesToBump),
                         op = opBumped,
                         rhs = bumpType(rhs, typesToBump))
      }
      case tWith @ Type.With(lhs, rhs) =>
        tWith.copy(bumpType(lhs, typesToBump), bumpType(rhs, typesToBump))
      case Type.Placeholder(Type.Bounds(lo, hi)) =>
        Type.Placeholder(
          Type.Bounds(lo.map(l => bumpType(l, typesToBump)),
                      hi.map(h => bumpType(h, typesToBump))))
      case Type.And(lhs, rhs)                 => Type.And(bumpType(lhs, typesToBump), bumpType(rhs, typesToBump))
      case Type.Or(lhs, rhs)                  => Type.Or(bumpType(lhs, typesToBump), bumpType(rhs, typesToBump))
      case typeAnnotate @ Type.Annotate(t, _) => typeAnnotate.copy(tpe = bumpType(t, typesToBump))
      case typeExist @ Type.Existential(t, _) => typeExist.copy(tpe = bumpType(t, typesToBump))
      case typeFunc @ Type.Function(params, res) => {
        val bumpedParams = params.map {
          case Type.Arg.ByName(t)   => Type.Arg.ByName(bumpType(t, typesToBump))
          case Type.Arg.Repeated(t) => Type.Arg.Repeated(bumpType(t, typesToBump))
          case t: Type => bumpType(t, typesToBump)
        }
        val bumpedRes = bumpType(res, typesToBump)
        typeFunc.copy(params = bumpedParams, res = bumpedRes)
      }
      case typeRefine @ Type.Refine(maybeTpe, _) =>
        typeRefine.copy(tpe = maybeTpe.map(t => bumpType(t, typesToBump)))
      case Type.Tuple(tpes) => Type.Tuple(tpes.map(t => bumpType(t, typesToBump)))
      case other            => other // Singleton and Project, I believe ...

    }

  }

}
