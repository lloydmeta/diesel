package diesel.internal

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
        val algebraName       = extracted.tname
        val algebraCtorParams = extracted.ctorParams
        val tparams           = extracted.tparams
        val template          = extracted.template
        val tparam            = selectOneFunctor(tparams)
        val builder = new TransformKMethBuilder(
          transKMethodName = transKMethodName,
          algebraType = algebraName,
          algebraCtorParams = algebraCtorParams,
          tparam = tparam,
          template = template,
          ctorRefBuilder = extracted.ctorCall
        )
        val meth = builder.build()
        extracted.appendStat(meth)
      }
      case Term.Block(Seq(SupportedAnnottee(extracted), companion)) => {
        val algebraName       = extracted.tname
        val algebraCtorParams = extracted.ctorParams
        val tparams           = extracted.tparams
        val template          = extracted.template
        val tparam            = selectOneFunctor(tparams)
        val builder = new TransformKMethBuilder(
          transKMethodName = transKMethodName,
          algebraType = algebraName,
          algebraCtorParams = algebraCtorParams,
          tparam = tparam,
          template = template,
          ctorRefBuilder = extracted.ctorCall
        )
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

  private def transKSuffixed(tpname: Type.Name): Type.Name = {
    val k = tpname.value
    Type.Name(s"$k$transKTypeSuffix")
  }

  private def selectOneFunctor(tparams: Seq[Type.Param]): Type.Param = tparams match {
    case Seq(tparam) if tparam.tparams.size == 1 => tparam
    case _ =>
      abort(
        s"This annotation only supports types parameterised with one kind that takes one type argument, but you provided $tparams")
  }

  // Holds a bunch of state that we don't want to keep passing around
  private class TransformKMethBuilder(transKMethodName: Term.Name,
                                      algebraType: Type.Name,
                                      algebraCtorParams: Seq[Seq[Term.Param]],
                                      tparam: Type.Param,
                                      template: Template,
                                      ctorRefBuilder: Type => Ctor.Call) {

    def build(): Defn.Def = {
      ensureSoundness()
      val forwardedAbstracts = forwardableAbstrats.flatMap {
        case origDef: Decl.Val => toForwardedDefnVals(origDef)
        case origDef: Decl.Def => Seq(toForwardedDefnDef(origDef))
        case _                 => abort("Oh noes! You found a bug in the macro! Please file an issue :)")
      }

      q"""
      final def $transKMethodName[$transformTargetK]($natTransArg: _root_.diesel.FunK[$tparamAsType, $targetKType]): $algebraType[$targetKType] = {
        val $currentTraitPat = $selfRefTerm
        new $algebraTargetKConstructor {
          ..$forwardedAbstracts
        }
      }"""
    }

    private def ensureSoundness(): Unit = {
      val ctorErrors = if (paramssParameterisedByKind(algebraCtorParams)) {
        def render(ps: Seq[Term.Param]) = {
          s"""(${ps.map(_.toString).mkString(", ")})"""
        }
        val paramssString = algebraCtorParams.map(render).mkString("")
        Seq(
          s"""Your algebra has constructor parameters with types referencing the algebra's Kind. Currently, this is not supported.
             |Please consider using a context bound instead (e.g. F[_]: Monad), which *is* supported.
             |
             |    ${algebraType.value}[${tparam.syntax}]$paramssString
           """.stripMargin)
      } else {
        Nil
      }

      val dslMembersSet      = (forwardableAbstrats: List[Stat]).toSet
      val concreteMembersSet = (concretes: List[Stat]).toSet
      // The spaces in multiline strings are significant
      val statsWithErrors = findErrors(
        Seq(
          (s"""Abstract methods with parameters that have types parameterised by the same kind as the annottee
               |      ($tparamName[_]) are not supported.""".stripMargin,
           paramsParameterisedByKind),
          ("Please use only package private modifiers for abstract members.",
           privateMembersPf(concreteMembersSet)),
          ("Return types must be explicitly stated.", noReturnTypePf(concreteMembersSet)),
          ("Abstract type members are not supported.", abstractType),
          (s"""The return type of this method references $tparamName[_] as a type argument.""".stripMargin,
           nonMatchingKindPf(dslMembersSet ++ concreteMembersSet)),
          ("Vars are not allowed.", varsPf(Set.empty)),
          ("Vals that are not assignments are not allowed at the moment.",
           patternMatchingVals(concreteMembersSet)),
          (s"Type member shadows the algebra's kind $tparamName[_] (same name or otherwise points to it).",
           typeMemberPointsToKind),
          (s"""This method has a type parameter that shadows the $tparamName[_] used to annotate the trait.
               |      Besides being confusing for readers of your code, this is not currently supported.""".stripMargin,
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
      val combinedErrMsgs = ctorErrors ++ specificErrors ++ genUnsupportedErrs
      if (combinedErrMsgs.nonEmpty) {
        val errsMsg = combinedErrMsgs.mkString("\n")
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

    private val tparamName       = tparam.name.value
    private val tparamAsType     = Type.fresh().copy(tparamName)
    private val targetKType      = transKSuffixed(Type.Name(tparamName))
    private val transformTargetK = tparam.copy(name = targetKType)

    private val templateStatements = template.stats.toSeq.flatten

    private val algebraTargetKConstructor = ctorRefBuilder(targetKType)

    private val concretes: List[Defn] =
      templateStatements.collect {
        case v: Defn => v
      }.toList

    private val forwardableAbstrats: List[Decl] =
      templateStatements.collect {
        case d: Decl.Def if algKindWrapped(d.decltpe) || !typeRefsAlgKind(d.decltpe) => d
        case v: Decl.Val if algKindWrapped(v.decltpe) || !typeRefsAlgKind(v.decltpe) => v
      }.toList

    private def algKindWrapped(t: Type): Boolean = t match {
      case Type.Apply(retName: Type.Select, tArgs)
          if retName.name.value == tparamName && !tArgs.map(typeRefsAlgKind).exists(identity) =>
        true
      case Type.Apply(retName: Type.Name, tArgs)
          if retName.value == tparamName && !tArgs.map(typeRefsAlgKind).exists(identity) =>
        true
      case _ => false
    }

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
      flattened.exists { param =>
        param.decltpe
          .map {
            case Type.Arg.ByName(t)   => typeRefsAlgKind(t)
            case Type.Arg.Repeated(t) => typeRefsAlgKind(t)
            case t: Type              => typeRefsAlgKind(t)
          }
          .exists(identity)
      }
    }

    private def abstractType: StatPF = {
      case t: Decl.Type => t
    }

    private def typeMemberPointsToKind: StatPF = {
      case t @ Defn.Type(_, Type.Name(n), _, Type.Apply(Type.Name(v), _))
          if v == tparamName || n == tparamName =>
        t
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

    private def toForwardedDefnVals(abstVal: Decl.Val): Seq[Defn.Val] = {
      val declTypeWrappedByAlgKind = algKindWrapped(abstVal.decltpe)
      val newdeclTpe =
        if (declTypeWrappedByAlgKind)
          suffixTypeNames(Set(tparamName))(abstVal.decltpe)
        else
          abstVal.decltpe
      abstVal.pats.map { pat =>
        val forwardingCall = q"""$currentTraitHandle.${pat.name}"""
        val body =
          if (declTypeWrappedByAlgKind)
            q"""$natTransArg.apply($forwardingCall)"""
          else
            forwardingCall
        Defn.Val(abstVal.mods, Seq(pat), Some(newdeclTpe), body)
      }
    }

    private def toForwardedDefnDef(abstrMeth: Decl.Def): Defn.Def = {
      val declTypeWrappedByAlgKind = algKindWrapped(abstrMeth.decltpe)

      val defWithTransKedTParams = addSuffixToTypeParams(abstrMeth)
      val mods                   = defWithTransKedTParams.mods
      val name                   = defWithTransKedTParams.name
      val tparams                = defWithTransKedTParams.tparams
      val paramss                = defWithTransKedTParams.paramss

      // Do not use KTrans wrapped return type if the original type was not wrapped in the
      // algebra kind.
      val declTpe = defWithTransKedTParams.decltpe
      val tparamTypes = tparams.map(tp => Type.Name(tp.name.value))
      val paramNames  = paramss.map(_.map(tp => Term.Name(tp.name.value)))
      val forwardingCall =
        if (tparamTypes.nonEmpty)
          q"""$currentTraitHandle.$name[..$tparamTypes](...$paramNames)"""
        else
          q"""$currentTraitHandle.$name(...$paramNames)"""
      val body =
        if (declTypeWrappedByAlgKind)
          q"""$natTransArg.apply($forwardingCall)"""
        else
          forwardingCall
      Defn.Def(mods, name, tparams, paramss, Some(declTpe), body)
    }

    private def addSuffixToTypeParams(meth: Decl.Def): Decl.Def = {
      // Add on the Kind param of the original algebra because we want it to be properly suffixed
      // when referenced to in the methods of our new algebra implementation.
      val tParamsToBump = meth.tparams.map(_.name.value).toSet

      def bumpTParam(tparam: Type.Param): Type.Param = {
        val nameStr: String = tparam.name.value
        val bumpedTparamName: Type.Param.Name =
          if (tParamsToBump.contains(nameStr))
            transKSuffixed(Type.Name(nameStr))
          else
            tparam.name
        val bumpedTParamTParams = tparam.tparams.map(tp => bumpTParam(tp))

        val bumpedCBounds = tparam.cbounds.map(suffixTypeNames(tParamsToBump))
        val bumpedVBounds = tparam.vbounds.map(suffixTypeNames(tParamsToBump))
        val bumpedTBounds = {
          val Type.Bounds(lo, hi) = tparam.tbounds
          Type.Bounds(lo.map(suffixTypeNames(tParamsToBump)),
                      hi.map(suffixTypeNames(tParamsToBump)))
        }
        tparam.copy(name = bumpedTparamName,
                    tparams = bumpedTParamTParams,
                    tbounds = bumpedTBounds,
                    vbounds = bumpedVBounds,
                    cbounds = bumpedCBounds)
      }

      val newtParams = meth.tparams.map { tparam =>
        bumpTParam(tparam)
      }
      // If the algebra kind wraps the return type, then suffix the kind too (G[_] -> GTransK[_])
      val newDeclTpe = if (algKindWrapped(meth.decltpe))
          suffixTypeNames(tParamsToBump + tparamName)(meth.decltpe)
        else
          suffixTypeNames(tParamsToBump)(meth.decltpe)
      val newParamss = meth.paramss.map { params =>
        params.map { param =>
          val bumpedTArg = param.decltpe.map(transformTArgType(suffixTypeNames(tParamsToBump)))
          param.copy(decltpe = bumpedTArg)
        }
      }
      meth.copy(tparams = newtParams, paramss = newParamss, decltpe = newDeclTpe)
    }

    // Type references algebra parameter Kind
    def typeRefsAlgKind(tpe: Type): Boolean = tpe match {
      case Type.Name(v) => v == tparamName
      case Type.Apply(tpeInner, args) =>
        typeRefsAlgKind(tpeInner) || args.exists(typeRefsAlgKind)
      case Type.ApplyInfix(lhs, opTName, rhs) => {
        typeRefsAlgKind(lhs) || typeRefsAlgKind(opTName) || typeRefsAlgKind(rhs)
      }
      case Type.With(lhs, rhs) => typeRefsAlgKind(lhs) || typeRefsAlgKind(rhs)
      case Type.Placeholder(Type.Bounds(lo, hi)) =>
        Seq(lo.map(typeRefsAlgKind), hi.map(typeRefsAlgKind)).flatten
          .exists(identity)
      case Type.And(lhs, rhs)     => typeRefsAlgKind(lhs) || typeRefsAlgKind(rhs)
      case Type.Or(lhs, rhs)      => typeRefsAlgKind(lhs) || typeRefsAlgKind(rhs)
      case Type.Annotate(t, _)    => typeRefsAlgKind(t)
      case Type.Existential(t, _) => typeRefsAlgKind(t)
      case Type.Function(params, res) => {
        val paramsRefsAlgKind = params.map {
          case Type.Arg.Repeated(t) => typeRefsAlgKind(t)
          case Type.Arg.ByName(t)   => typeRefsAlgKind(t)
          case t: Type              => typeRefsAlgKind(t)
        }
        typeRefsAlgKind(res) || paramsRefsAlgKind.exists(identity)
      }
      case Type.Refine(maybeTpe, _) =>
        maybeTpe.map(typeRefsAlgKind).exists(identity)
      case Type.Tuple(tpes)              => tpes.map(typeRefsAlgKind).exists(identity)
      case Type.Project(q, Type.Name(v)) => v == tparamName || typeRefsAlgKind(q)
      case Type.Select(_, Type.Name(v))  => v == tparamName
      case _                             => false // Singleton, I believe ... which can't point to method type params ?
    }

    private def transformTArgType(f: Type => Type)(tArg: Type.Arg): Type.Arg = tArg match {
      case Type.Arg.Repeated(tpe) => Type.Arg.Repeated(f(tpe))
      case Type.Arg.ByName(tpe)   => Type.Arg.ByName(f(tpe))
      case t: Type                => f(t)
    }

    // Adds a suffix to a type if it matches any of the names given
    def suffixTypeNames(tNamesToSuffix: Set[String])(tpe: Type): Type = {
      def suffixTypeInner(tpe: Type): Type = tpe match {
        case tName @ Type.Name(v) if tNamesToSuffix.contains(v) => transKSuffixed(tName)
        case tApply @ Type.Apply(tpeInner, args) =>
          tApply.copy(tpe = suffixTypeInner(tpeInner), args = args.map(a => suffixTypeInner(a)))
        case tApplyInfix @ Type.ApplyInfix(lhs, opTName @ Type.Name(op), rhs) => {
          val opBumped =
            if (tNamesToSuffix.contains(op))
              transKSuffixed(opTName)
            else
              Type.Name(op)
          tApplyInfix.copy(lhs = suffixTypeInner(lhs), op = opBumped, rhs = suffixTypeInner(rhs))
        }
        case tWith @ Type.With(lhs, rhs) =>
          tWith.copy(lhs = suffixTypeInner(lhs), rhs = suffixTypeInner(rhs))
        case Type.Placeholder(Type.Bounds(lo, hi)) =>
          Type.Placeholder(
            Type.Bounds(lo = lo.map(l => suffixTypeInner(l)),
                        hi = hi.map(h => suffixTypeInner(h))))
        case Type.And(lhs, rhs) => Type.And(lhs = suffixTypeInner(lhs), rhs = suffixTypeInner(rhs))
        case Type.Or(lhs, rhs)  => Type.Or(lhs = suffixTypeInner(lhs), rhs = suffixTypeInner(rhs))
        case typeAnnotate @ Type.Annotate(t, _) =>
          typeAnnotate.copy(tpe = suffixTypeInner(t))
        case typeExist @ Type.Existential(t, _) => typeExist.copy(tpe = suffixTypeInner(t))
        case typeFunc @ Type.Function(params, res) => {
          val bumpedParams = params.map(transformTArgType(suffixTypeInner))
          val bumpedRes    = suffixTypeInner(res)
          typeFunc.copy(params = bumpedParams, res = bumpedRes)
        }
        case typeRefine @ Type.Refine(maybeTpe, _) =>
          typeRefine.copy(tpe = maybeTpe.map(t => suffixTypeInner(t)))
        case Type.Tuple(tpes) => Type.Tuple(tpes.map(t => suffixTypeInner(t)))
        case Type.Project(q, tName @ Type.Name(v)) if tNamesToSuffix.contains(v) =>
          Type.Project(qual = suffixTypeInner(q), name = transKSuffixed(tName))
        case Type.Select(r, tName @ Type.Name(v)) if tNamesToSuffix.contains(v) =>
          Type.Select(r, transKSuffixed(tName))
        case other => other // Singleton, I believe ... which can't point to method type params ?
      }
      suffixTypeInner(tpe)
    }
  }

}
