package dotty.tools
package dotc
package typer

import core._
import ast._
import Contexts._, Types._, Flags._, Symbols._
import Trees._
import ProtoTypes._
import NameKinds.UniqueName
import util.Spans._
import util.{Stats, SimpleIdentityMap}
import Decorators._
import config.Printers.{gadts, typr}
import annotation.tailrec
import reporting._
import collection.mutable

import scala.annotation.internal.sharable
import scala.annotation.threadUnsafe

import config.Printers.gadts

object Inferencing {

  import tpd._

  /** Is type fully defined, meaning the type does not contain wildcard types
   *  or uninstantiated type variables. As a side effect, this will minimize
   *  any uninstantiated type variables, according to the given force degree,
   *  but only if the overall result of `isFullyDefined` is `true`.
   *  Variables that are successfully minimized do not count as uninstantiated.
   */
  def isFullyDefined(tp: Type, force: ForceDegree.Value)(using Context): Boolean = {
    val nestedCtx = ctx.fresh.setNewTyperState()
    val result = new IsFullyDefinedAccumulator(force)(using nestedCtx).process(tp)
    if (result) nestedCtx.typerState.commit()
    result
  }

  /** The fully defined type, where all type variables are forced.
   *  Throws an error if type contains wildcards.
   */
  def fullyDefinedType(tp: Type, what: String, span: Span)(using Context): Type =
    if (isFullyDefined(tp, ForceDegree.all)) tp
    else throw new Error(i"internal error: type of $what $tp is not fully defined, pos = $span") // !!! DEBUG

  /** Instantiate selected type variables `tvars` in type `tp` in a special mode:
   *   1. If a type variable is constrained from below (i.e. constraint bound != given lower bound)
   *      it is minimized.
   *   2. Otherwise, if the type variable is constrained from above, it is maximized.
   *   3. Otherwise, if the type variable has a lower bound != Nothing, it is minimized.
   *   4. Otherwise, if the type variable has an upper bound != Any, it is maximized.
   *  If none of (1) - (4) applies, the type variable is left uninstantiated.
   *  The method is called to instantiate type variables before an implicit search.
   */
  def instantiateSelected(tp: Type, tvars: List[Type])(using Context): Unit =
    if (tvars.nonEmpty)
      IsFullyDefinedAccumulator(
        ForceDegree.Value(tvars.contains, IfBottom.flip), minimizeSelected = true
      ).process(tp)

  /** Instantiate any type variables in `tp` whose bounds contain a reference to
   *  one of the parameters in `tparams` or `vparamss`.
   */
  def instantiateDependent(tp: Type, tparams: List[Symbol], vparamss: List[List[Symbol]])(using Context): Unit = {
    val dependentVars = new TypeAccumulator[Set[TypeVar]] {
      @threadUnsafe lazy val params = (tparams :: vparamss).flatten
      def apply(tvars: Set[TypeVar], tp: Type) = tp match {
        case tp: TypeVar
        if !tp.isInstantiated &&
            accCtx.typeComparer.bounds(tp.origin)
              .namedPartsWith(ref => params.contains(ref.symbol))
              .nonEmpty =>
          tvars + tp
        case _ =>
          foldOver(tvars, tp)
      }
    }
    val depVars = dependentVars(Set(), tp)
    if (depVars.nonEmpty) instantiateSelected(tp, depVars.toList)
  }

  /** The accumulator which forces type variables using the policy encoded in `force`
   *  and returns whether the type is fully defined. The direction in which
   *  a type variable is instantiated is determined as follows:
   *   1. T is minimized if the constraint over T is only from below (i.e.
   *      constrained lower bound != given lower bound and
   *      constrained upper bound == given upper bound).
   *   2. T is maximized if the constraint over T is only from above (i.e.
   *      constrained upper bound != given upper bound and
   *      constrained lower bound == given lower bound).
   *
   *  If (1) and (2) do not apply, and minimizeSelected is set:
   *   3. T is minimized if it has a lower bound (different from Nothing) in the
   *      current constraint (the bound might come from T's declaration).
   *   4. Otherwise, T is maximized if it has an upper bound (different from Any)
   *      in the currented constraint (the bound might come from T's declaration).
   *   5. Otherwise, T is not instantiated at all.

   *  If (1) and (2) do not apply, and minimizeSelected is not set:
   *   6: T is maximized if it appears only contravariantly in the given type,
   *      or if forceDegree is `flipBottom` and T has no lower bound different from Nothing.
   *   7. Otherwise, T is minimized.
   *
   *  The instantiation for (6) and (7) is done in two phases:
   *  1st Phase: Try to instantiate minimizable type variables to
   *  their lower bound. Record whether successful.
   *  2nd Phase: If first phase was successful, instantiate all remaining type variables
   *  to their upper bound.
   */
  private class IsFullyDefinedAccumulator(force: ForceDegree.Value, minimizeSelected: Boolean = false)
    (using Context) extends TypeAccumulator[Boolean] {

    private def instantiate(tvar: TypeVar, fromBelow: Boolean): Type = {
      val inst = tvar.instantiate(fromBelow)
      typr.println(i"forced instantiation of ${tvar.origin} = $inst")
      inst
    }

    private var toMaximize: List[TypeVar] = Nil

    def apply(x: Boolean, tp: Type): Boolean = tp.dealias match {
      case _: WildcardType | _: ProtoType =>
        false
      case tvar: TypeVar if !tvar.isInstantiated =>
        force.appliesTo(tvar)
        && ctx.typerState.constraint.contains(tvar)
        && {
          val direction = instDirection(tvar.origin)
          if direction != 0 then
            instantiate(tvar, fromBelow = direction < 0)
          else if minimizeSelected then
            if tvar.hasLowerBound then instantiate(tvar, fromBelow = true)
            else if tvar.hasUpperBound then instantiate(tvar, fromBelow = false)
            else () // hold off instantiating unbounded unconstrained variables
          else if variance >= 0 && (force.ifBottom == IfBottom.ok || tvar.hasLowerBound) then
            instantiate(tvar, fromBelow = true)
          else if variance >= 0 && force.ifBottom == IfBottom.fail then
            return false
          else
            toMaximize = tvar :: toMaximize
          foldOver(x, tvar)
        }
      case tp =>
        foldOver(x, tp)
    }

    def process(tp: Type): Boolean =
      // Maximize type vars in the order they were visited before */
      def maximize(tvars: List[TypeVar]): Unit = tvars match
        case tvar :: tvars1 =>
          maximize(tvars1)
          if !tvar.isInstantiated then
            instantiate(tvar, fromBelow = false)
        case nil =>
      apply(true, tp)
      && (
        toMaximize.isEmpty
        || { maximize(toMaximize)
             toMaximize = Nil       // Do another round since the maximixing instances
             process(tp)            // might have type uninstantiated variables themselves.
           }
      )
  }

  def approximateGADT(tp: Type)(implicit ctx: Context): Type = {
    val map = new ApproximateGadtAccumulator
    val res = map(tp)
    assert(!map.failed)
    gadts.println(i"approximateGADT( $tp )  = $res  //  {${tp.toString}}") // FIXME remove
    res
  }

  /** This class is mostly based on IsFullyDefinedAccumulator.
    * It tries to approximate the given type based on the available GADT constraints.
    */
  private class ApproximateGadtAccumulator(implicit ctx: Context) extends TypeMap {

    var failed = false

    private def instantiate(tvar: TypeVar, fromBelow: Boolean): Type = {
      val inst = tvar.instantiate(fromBelow)
      typr.println(i"forced instantiation of ${tvar.origin} = $inst")
      inst
    }

    private def instDirection2(sym: Symbol)(implicit ctx: Context): Int = {
      val constrained = ctx.gadt.fullBounds(sym)
      val original = sym.info.bounds
      val cmp = ctx.typeComparer
      val approxBelow =
        if (!cmp.isSubTypeWhenFrozen(constrained.lo, original.lo)) 1 else 0
      val approxAbove =
        if (!cmp.isSubTypeWhenFrozen(original.hi, constrained.hi)) 1 else 0
      approxAbove - approxBelow
    }

    private[this] var toMaximize: Boolean = false

    def apply(tp: Type): Type = tp.dealias match {
      case tp @ TypeRef(qual, nme) if (qual eq NoPrefix) && ctx.gadt.contains(tp.symbol) =>
        val sym = tp.symbol
        val res =
          ctx.gadt.approximation(sym, fromBelow = variance < 0)
        gadts.println(i"approximated $tp  ~~  $res")
        res

      case _: WildcardType | _: ProtoType =>
        failed = true
        NoType

      case tp =>
        mapOver(tp)
    }

    def process(tp: Type): Type = {
      apply(tp)
    }
  }

  /** For all type parameters occurring in `tp`:
   *  If the bounds of `tp` in the current constraint are equal wrt =:=,
   *  instantiate the type parameter to the lower bound's approximation
   *  (approximation because of possible F-bounds).
   */
  def replaceSingletons(tp: Type)(using Context): Unit = {
    val tr = new TypeTraverser {
      def traverse(tp: Type): Unit = {
        tp match {
          case param: TypeParamRef =>
            val constraint = accCtx.typerState.constraint
            constraint.entry(param) match {
              case TypeBounds(lo, hi)
              if (hi frozen_<:< lo) =>
                val inst = accCtx.typeComparer.approximation(param, fromBelow = true)
                typr.println(i"replace singleton $param := $inst")
                accCtx.typerState.constraint = constraint.replace(param, inst)
              case _ =>
            }
          case _ =>
        }
        traverseChildren(tp)
      }
    }
    tr.traverse(tp)
  }

  /** If `tree` has a type lambda type, infer its type parameters by comparing with expected type `pt` */
  def inferTypeParams(tree: Tree, pt: Type)(using Context): Tree = tree.tpe match {
    case tl: TypeLambda =>
      val (tl1, tvars) = constrained(tl, tree)
      var tree1 = AppliedTypeTree(tree.withType(tl1), tvars)
      tree1.tpe <:< pt
      fullyDefinedType(tree1.tpe, "template parent", tree.span)
      tree1
    case _ =>
      tree
  }

  def isSkolemFree(tp: Type)(using Context): Boolean =
    !tp.existsPart(_.isInstanceOf[SkolemType])

  /** The list of uninstantiated type variables bound by some prefix of type `T` which
   *  occur in at least one formal parameter type of a prefix application.
   *  Considered prefixes are:
   *    - The function `f` of an application node `f(e1, .., en)`
   *    - The function `f` of a type application node `f[T1, ..., Tn]`
   *    - The prefix `p` of a selection `p.f`.
   *    - The result expression `e` of a block `{s1; .. sn; e}`.
   */
  def tvarsInParams(tree: Tree, locked: TypeVars)(using Context): List[TypeVar] = {
    @tailrec def boundVars(tree: Tree, acc: List[TypeVar]): List[TypeVar] = tree match {
      case Apply(fn, _) => boundVars(fn, acc)
      case TypeApply(fn, targs) =>
        val tvars = targs.filter(_.isInstanceOf[TypeVarBinder[?]]).tpes.collect {
          case tvar: TypeVar
          if !tvar.isInstantiated &&
             ctx.typerState.ownedVars.contains(tvar) &&
             !locked.contains(tvar) => tvar
        }
        boundVars(fn, acc ::: tvars)
      case Select(pre, _) => boundVars(pre, acc)
      case Block(_, expr) => boundVars(expr, acc)
      case _ => acc
    }
    @tailrec def occurring(tree: Tree, toTest: List[TypeVar], acc: List[TypeVar]): List[TypeVar] =
      if (toTest.isEmpty) acc
      else tree match {
        case Apply(fn, _) =>
          fn.tpe.widen match {
            case mtp: MethodType =>
              val (occ, nocc) = toTest.partition(tvar => mtp.paramInfos.exists(tvar.occursIn))
              occurring(fn, nocc, occ ::: acc)
            case _ =>
              occurring(fn, toTest, acc)
          }
        case TypeApply(fn, targs) => occurring(fn, toTest, acc)
        case Select(pre, _) => occurring(pre, toTest, acc)
        case Block(_, expr) => occurring(expr, toTest, acc)
        case _ => acc
      }
    occurring(tree, boundVars(tree, Nil), Nil)
  }

  /** The instantiation direction for given poly param computed
   *  from the constraint:
   *  @return   1 (maximize) if constraint is uniformly from above,
   *           -1 (minimize) if constraint is uniformly from below,
   *            0 if unconstrained, or constraint is from below and above.
   */
  private def instDirection(param: TypeParamRef)(using Context): Int = {
    val constrained = ctx.typeComparer.fullBounds(param)
    val original = param.binder.paramInfos(param.paramNum)
    val cmp = ctx.typeComparer
    val approxBelow =
      if (!cmp.isSubTypeWhenFrozen(constrained.lo, original.lo)) 1 else 0
    val approxAbove =
      if (!cmp.isSubTypeWhenFrozen(original.hi, constrained.hi)) 1 else 0
    approxAbove - approxBelow
  }

  /** Following type aliases and stripping refinements and annotations, if one arrives at a
   *  class type reference where the class has a companion module, a reference to
   *  that companion module. Otherwise NoType
   */
  def companionRef(tp: Type)(using Context): Type =
    tp.underlyingClassRef(refinementOK = true) match {
      case tp: TypeRef =>
        val companion = tp.classSymbol.companionModule
        if (companion.exists)
          companion.termRef.asSeenFrom(tp.prefix, companion.owner)
        else NoType
      case _ => NoType
    }

  /** Instantiate undetermined type variables so that type `tp` is maximized.
   *  @return   The list of type symbols that were created
   *            to instantiate undetermined type variables that occur non-variantly
   */
  def maximizeType(tp: Type, span: Span, fromScala2x: Boolean)(using Context): List[Symbol] = {
    Stats.record("maximizeType")
    val vs = variances(tp)
    val patternBound = new mutable.ListBuffer[Symbol]
    vs foreachBinding { (tvar, v) =>
      if (v == 1) tvar.instantiate(fromBelow = false)
      else if (v == -1) tvar.instantiate(fromBelow = true)
      else {
        val bounds = ctx.typeComparer.fullBounds(tvar.origin)
        if (bounds.hi <:< bounds.lo || bounds.hi.classSymbol.is(Final) || fromScala2x)
          tvar.instantiate(fromBelow = false)
        else {
          // We do not add the created symbols to GADT constraint immediately, since they may have inter-dependencies.
          // Instead, we simultaneously add them later on.
          val wildCard = ctx.newPatternBoundSymbol(UniqueName.fresh(tvar.origin.paramName), bounds, span, addToGadt = false)
          tvar.instantiateWith(wildCard.typeRef)
          patternBound += wildCard
        }
      }
    }
    val res = patternBound.toList
    // We add the created symbols to GADT constraint here.
    if (res.nonEmpty) ctx.gadt.addToConstraint(res)
    res
  }

  type VarianceMap = SimpleIdentityMap[TypeVar, Integer]

  /** All occurrences of type vars in this type that satisfy predicate
   *  `include` mapped to their variances (-1/0/1) in this type, where
   *  -1 means: only covariant occurrences
   *  +1 means: only covariant occurrences
   *  0 means: mixed or non-variant occurrences
   *
   *  Note: We intentionally use a relaxed version of variance here,
   *  where the variance does not change under a prefix of a named type
   *  (the strict version makes prefixes invariant). This turns out to be
   *  better for type inference. In a nutshell, if a type variable occurs
   *  like this:
   *
   *     (U? >: x.type) # T
   *
   *  we want to instantiate U to x.type right away. No need to wait further.
   */
  private def variances(tp: Type)(using Context): VarianceMap = {
    Stats.record("variances")
    val constraint = ctx.typerState.constraint

    object accu extends TypeAccumulator[VarianceMap] {
      def setVariance(v: Int) = variance = v
      def apply(vmap: VarianceMap, t: Type): VarianceMap = t match {
        case t: TypeVar
        if !t.isInstantiated && accCtx.typerState.constraint.contains(t) =>
          val v = vmap(t)
          if (v == null) vmap.updated(t, variance)
          else if (v == variance || v == 0) vmap
          else vmap.updated(t, 0)
        case _ =>
          foldOver(vmap, t)
      }
    }

    /** Include in `vmap` type variables occurring in the constraints of type variables
     *  already in `vmap`. Specifically:
     *   - if `tvar` is covariant in `vmap`, include all variables in its lower bound
     *     (because they influence the minimal solution of `tvar`),
     *   - if `tvar` is contravariant in `vmap`, include all variables in its upper bound
     *     at flipped variances (because they influence the maximal solution of `tvar`),
     *   - if `tvar` is nonvariant in `vmap`, include all variables in its upper and lower
     *     bounds as non-variant.
     *  Do this in a fixpoint iteration until `vmap` stabilizes.
     */
    def propagate(vmap: VarianceMap): VarianceMap = {
      var vmap1 = vmap
      def traverse(tp: Type) = { vmap1 = accu(vmap1, tp) }
      vmap.foreachBinding { (tvar, v) =>
        val param = tvar.origin
        val e = constraint.entry(param)
        accu.setVariance(v)
        if (v >= 0) {
          traverse(e.bounds.lo)
          constraint.lower(param).foreach(p => traverse(constraint.typeVarOfParam(p)))
        }
        if (v <= 0) {
          traverse(e.bounds.hi)
          constraint.upper(param).foreach(p => traverse(constraint.typeVarOfParam(p)))
        }
      }
      if (vmap1 eq vmap) vmap else propagate(vmap1)
    }

    propagate(accu(SimpleIdentityMap.Empty, tp))
  }
}

trait Inferencing { this: Typer =>
  import Inferencing._
  import tpd._

  /** Interpolate undetermined type variables in the widened type of this tree.
   *  @param tree    the tree whose type is interpolated
   *  @param pt      the expected result type
   *  @param locked  the set of type variables of the current typer state that cannot be interpolated
   *                 at the present time
   *  Eligible for interpolation are all type variables owned by the current typerstate
   *  that are not in locked. Type variables occurring co- (respectively, contra-) variantly in the type
   *  are minimized (respectvely, maximized). Non occurring type variables are minimized if they
   *  have a lower bound different from Nothing, maximized otherwise. Type variables appearing
   *  non-variantly in the type are left untouched.
   *
   *  Note that even type variables that do not appear directly in a type, can occur with
   *  some variance in the type, because of the constraints. E.g if `X` occurs co-variantly in `T`
   *  and we have a constraint
   *
   *      Y <: X
   *
   *  Then `Y` also occurs co-variantly in `T` because it needs to be minimized in order to constrain
   *  `T` the least. See `variances` for more detail.
   */
  def interpolateTypeVars(tree: Tree, pt: Type, locked: TypeVars)(using Context): tree.type = {
    val state = ctx.typerState

    // Note that some variables in `locked` might not be in `state.ownedVars`
    // anymore if they've been garbage-collected, so we can't use
    // `state.ownedVars.size > locked.size` as an early check to avoid computing
    // `qualifying`.

    val ownedVars = state.ownedVars
    if ((ownedVars ne locked) && !ownedVars.isEmpty) {
      val qualifying = ownedVars -- locked
      if (!qualifying.isEmpty) {
        typr.println(i"interpolate $tree: ${tree.tpe.widen} in $state, owned vars = ${state.ownedVars.toList}%, %, qualifying = ${qualifying.toList}%, %, previous = ${locked.toList}%, % / ${state.constraint}")
        val resultAlreadyConstrained =
          tree.isInstanceOf[Apply] || tree.tpe.isInstanceOf[MethodOrPoly]
        if (!resultAlreadyConstrained)
          constrainResult(tree.symbol, tree.tpe, pt)
            // This is needed because it could establish singleton type upper bounds. See i2998.scala.

        val tp = tree.tpe.widen
        val vs = variances(tp)

        // Avoid interpolating variables occurring in tree's type if typerstate has unreported errors.
        // Reason: The errors might reflect unsatisfiable constraints. In that
        // case interpolating without taking account the constraints risks producing
        // nonsensical types that then in turn produce incomprehensible errors.
        // An example is in neg/i1240.scala. Without the condition in the next code line
        // we get for
        //
        //      val y: List[List[String]] = List(List(1))
        //
        //     i1430.scala:5: error: type mismatch:
        //     found   : Int(1)
        //     required: Nothing
        //     val y: List[List[String]] = List(List(1))
        //                                           ^
        // With the condition, we get the much more sensical:
        //
        //     i1430.scala:5: error: type mismatch:
        //     found   : Int(1)
        //     required: String
        //     val y: List[List[String]] = List(List(1))
        val hasUnreportedErrors = state.reporter.hasUnreportedErrors
        def constraint = state.constraint
        type InstantiateQueue = mutable.ListBuffer[(TypeVar, Boolean)]
        val toInstantiate = new InstantiateQueue
        for (tvar <- qualifying)
          if (!tvar.isInstantiated && constraint.contains(tvar)) {
            // Needs to be checked again, since previous interpolations could already have
            // instantiated `tvar` through unification.
            val v = vs(tvar)
            if (v == null) {
              typr.println(i"interpolate non-occurring $tvar in $state in $tree: $tp, fromBelow = ${tvar.hasLowerBound}, $constraint")
              toInstantiate += ((tvar, tvar.hasLowerBound))
            }
            else if (!hasUnreportedErrors)
              if (v.intValue != 0) {
                typr.println(i"interpolate $tvar in $state in $tree: $tp, fromBelow = ${v.intValue == 1}, $constraint")
                toInstantiate += ((tvar, v.intValue == 1))
              }
              else typr.println(i"no interpolation for nonvariant $tvar in $state")
          }

        /** Instantiate all type variables in `buf` in the indicated directions.
         *  If a type variable A is instantiated from below, and there is another
         *  type variable B in `buf` that is known to be smaller than A, wait and
         *  instantiate all other type variables before trying to instantiate A again.
         *  Dually, wait instantiating a type variable from above as long as it has
         *  upper bounds in `buf`.
         *
         *  This is done to avoid loss of precision when forming unions. An example
         *  is in i7558.scala:
         *
         *      type Tr[+V1, +O1 <: V1]
         *      def [V2, O2 <: V2](tr: Tr[V2, O2]) sl: Tr[V2, O2] = ???
         *      def as[V3, O3 <: V3](tr: Tr[V3, O3]) : Tr[V3, O3] = tr.sl
         *
         *   Here we interpolate at some point V2 and O2 given the constraint
         *
         *      V2 >: V3, O2 >: O3, O2 <: V2
         *
         *   where O3 and V3 are type refs with O3 <: V3.
         *   If we interpolate V2 first to V3 | O2, the widenUnion algorithm will
         *   instantiate O2 to V3, leading to the final constraint
         *
         *      V2 := V3, O2 := V3
         *
         *   But if we instantiate O2 first to O3, and V2 next to V3, we get the
         *   more flexible instantiation
         *
         *      V2 := V3, O2 := O3
         */
        def doInstantiate(buf: InstantiateQueue): Unit =
          if buf.nonEmpty then
            val suspended = new InstantiateQueue
            while buf.nonEmpty do
              val first @ (tvar, fromBelow) = buf.head
              buf.dropInPlace(1)
              val suspend = buf.exists{ (following, _) =>
                if fromBelow then
                  constraint.isLess(following.origin, tvar.origin)
                else
                  constraint.isLess(tvar.origin, following.origin)
              }
              if suspend then suspended += first else tvar.instantiate(fromBelow)
            doInstantiate(suspended)
        end doInstantiate
        doInstantiate(toInstantiate)
      }
    }
    tree
  }
}

/** An enumeration controlling the degree of forcing in "is-dully-defined" checks. */
@sharable object ForceDegree {
  class Value(val appliesTo: TypeVar => Boolean, val ifBottom: IfBottom)
  val none: Value = new Value(_ => false, IfBottom.ok)
  val all: Value = new Value(_ => true, IfBottom.ok)
  val failBottom: Value = new Value(_ => true, IfBottom.fail)
  val flipBottom: Value = new Value(_ => true, IfBottom.flip)
}

enum IfBottom:
  case ok, fail, flip

