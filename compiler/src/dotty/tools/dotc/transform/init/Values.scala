package dotty.tools.dotc
package transform
package init

import core._
import Contexts.Context
import StdNames._
import Names._
import NameKinds.DefaultGetterName
import ast._
import Trees._
import Symbols._
import Types._
import Decorators._
import util.Positions._
import Annotations._
import config.Printers.init.{ println => debug }
import collection.mutable

//=======================================
//             values
//=======================================

object Value {
  def checkParams(sym: Symbol, paramInfos: List[Type], values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = {
    paramInfos.zipWithIndex.foreach { case (tp, index) =>
      val value = scala.util.Try(values(index)).getOrElse(HotValue)
      val pos = scala.util.Try(argPos(index)).getOrElse(NoPosition)
      if (value.widen < tp.value)
        return Res(effects = Vector(Generic("Leak of object under initialization to " + sym.show, pos)))
    }
    Res()
  }

  def defaultFunctionValue(methSym: Symbol)(implicit setting: Setting): Value = {
    assert(methSym.is(Flags.Method))
    if (methSym.info.paramNamess.isEmpty) HotValue
    else new FunctionValue() {
      def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = {
        val paramInfos = methSym.info.paramInfoss.flatten
        checkParams(methSym, paramInfos, values, argPos)
      }
    }
  }
}

/** Abstract values in analysis */
sealed trait Value {
  /** Select a member on a value */
  def select(sym: Symbol, isStaticDispatch: Boolean = false)(implicit setting: Setting): Res

  /** Assign on a value */
  def assign(sym: Symbol, value: Value)(implicit setting: Setting): Res

  /** Index an inner class with current value as the immediate outer */
  def init(constr: Symbol, values: List[Value], argPos: List[Position], obj: ObjectValue)(implicit setting: Setting): Res

  /** Apply a method or function to the provided arguments */
  def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res

  def show(implicit setting: ShowSetting): String

  /** Join two values
   *
   *  NoValue < Cold < Warm < Hot
   */
  def join(other: Value): Value = (this, other) match {
    case (HotValue, v) => v
    case (v, HotValue) => v
    case (NoValue, _) => NoValue
    case (_, NoValue) => NoValue
    case (BlankValue, _) => BlankValue
    case (_, BlankValue) => BlankValue
    case (ColdValue, _) => ColdValue
    case (_, ColdValue) => ColdValue
    case (v1: OpaqueValue, v2: OpaqueValue)     => v1.join(v2)
    case (o1: ObjectValue, o2: ObjectValue) if o1 `eq` o2 => o1
    case (f1: FunctionValue, f2: FunctionValue) if f1 `eq` f2 => f1
    case (f1: FunctionValue, f2: FunctionValue) => f1.join(f2)
    case (o1: SliceValue, o2: SliceValue) =>
      if (o1.id == o2.id) o1
      else new UnionValue(Set(o1, o2))
    case (v1: LazyValue, v2: LazyValue) if v1 == v2 => v1
    case (v1: UnionValue, v2: UnionValue) => v1 ++ v2
    case (uv: UnionValue, v: SingleValue) => uv + v
    case (v: SingleValue, uv: UnionValue) => uv + v
    case (v1: SingleValue, v2: SingleValue) => UnionValue(Set(v1, v2))
  }

  /** Widen the value to an opaque value
   *
   *  Widening is needed at analysis boundary.
   */
  def widen(implicit setting: Setting): OpaqueValue = {
    def recur(value: Value)(implicit setting: Setting): OpaqueValue = value match {
      case ov: OpaqueValue => ov
      case fv: FunctionValue =>
        val setting2 = setting.freshHeap
        val res = fv(i => HotValue, i => NoPosition)(setting2)
        if (res.hasErrors) WarmValue
        else recur(res.value)(setting2)
      case sv: SliceValue =>
        setting.heap(sv.id).asSlice.widen
      case ov: ObjectValue =>
        if (ov.open) ColdValue
        else ov.slices.values.foldLeft(HotValue: OpaqueValue) { (acc, v) =>
          if (acc != HotValue) return WarmValue
          recur(v).join(acc)
        }
      case UnionValue(vs) =>
        vs.foldLeft(HotValue: OpaqueValue) { (acc, v) =>
          if (v == ColdValue || acc == ColdValue) return ColdValue
          else acc.join(recur(v))
        }
      // case NoValue => NoValue
      case _ => // impossible
        ???
    }

    recur(this)
  }
}

/** The value is absent */
object NoValue extends Value {
  def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = ???
  def select(sym: Symbol, isStaticDispatch: Boolean)(implicit setting: Setting): Res = ???
  def assign(sym: Symbol, value: Value)(implicit setting: Setting): Res = ???
  def init(constr: Symbol, values: List[Value], argPos: List[Position], obj: ObjectValue)(implicit setting: Setting): Res = ???

  def show(implicit setting: ShowSetting): String = "NoValue"
}

/** A single value, instead of a union value */
sealed trait SingleValue extends Value

/** Union of values */
case class UnionValue(val values: Set[SingleValue]) extends Value {
  def apply(args: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = {
    values.foldLeft(Res()) { (acc, value) =>
      value.apply(args, argPos).join(acc)
    }
  }

  def select(sym: Symbol, isStaticDispatch: Boolean)(implicit setting: Setting): Res = {
    values.foldLeft(Res()) { (acc, value) =>
      value.select(sym, isStaticDispatch).join(acc)
    }
  }

  def assign(sym: Symbol, value: Value)(implicit setting: Setting): Res = {
    values.foldLeft(Res()) { (acc, value) =>
      value.assign(sym, value).join(acc)
    }
  }

  def init(constr: Symbol, values: List[Value], argPos: List[Position], obj: ObjectValue)(implicit setting: Setting): Res = {
    values.foldLeft(Res()) { (acc, value) =>
      value.init(constr, values, argPos, obj).join(acc)
    }
  }

  def +(value: SingleValue): UnionValue = UnionValue(values + value)
  def ++(uv: UnionValue): UnionValue = UnionValue(values ++ uv.values)

  def show(implicit setting: ShowSetting): String =
    "Or{" + setting.indent(values.map(v => v.show(setting)).mkString(", ")) + "}"
}

/** Values that are subject to type checking rather than analysis */
abstract sealed class OpaqueValue extends SingleValue {
  // not supported
  def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = ???

  def <(that: OpaqueValue): Boolean = (this, that) match {
    case (HotValue, _) => false
    case (WarmValue, ColdValue | WarmValue) => false
    case (ColdValue, ColdValue) => false
    case _ => true
  }

  def join(that: OpaqueValue): OpaqueValue =
    if (this < that) this else that

  def meet(that: OpaqueValue): OpaqueValue =
    if (this < that) that else this
}

object HotValue extends OpaqueValue {
  def select(sym: Symbol, isStaticDispatch: Boolean)(implicit setting: Setting): Res =
    if (sym.is(Flags.Method)) Res(value = Value.defaultFunctionValue(sym))
    else Res()

  def assign(sym: Symbol, value: Value)(implicit setting: Setting): Res =
    if (value.widen != HotValue)
      Res(effects = Vector(Generic("Cannot assign an object under initialization to a full object", setting.pos)))
    else Res()

  def init(constr: Symbol, values: List[Value], argPos: List[Position], obj: ObjectValue)(implicit setting: Setting): Res = {
    val cls = constr.owner.asClass
    val paramInfos = constr.info.paramInfoss.flatten
    val res = Value.checkParams(cls, paramInfos, values, argPos)
    if (res.hasErrors) return res

    val args = (0 until paramInfos.size).map(i => scala.util.Try(values(i)).getOrElse(HotValue))
    if (args.exists(_.widen < HotValue)) obj.add(cls, WarmValue)

    Res()
  }

  def show(implicit setting: ShowSetting): String = "Hot"

  override def toString = "hot value"
}

/** A blank value, where class/trait params are not yet initialized
 *
 *  abstract class A {
 *    def f: Int
 *    val a = f
 *  }
 *
 *  trait B(x: 20) {
 *    def f: Int = x       // error: `x` is not initialized yet
 *  }
 *
 *  class C extends A with B(20)
 */
object BlankValue extends OpaqueValue {
  def select(sym: Symbol, isStaticDispatch: Boolean)(implicit setting: Setting): Res = {
    // set state to Hot, don't report same error message again
    val res = Res(value = HotValue)

    if (sym.is(Flags.Method)) {
      if (!sym.isCold && !sym.name.is(DefaultGetterName))
        res += Generic(s"The $sym should be marked as `@cold` in order to be called", setting.pos)

      res.value = Value.defaultFunctionValue(sym)
    }
    else if (sym.is(Flags.Lazy)) {
      if (!sym.isCold)
        res += Generic(s"The lazy field $sym should be marked as `@cold` in order to be accessed", setting.pos)
    }
    else if (sym.isClass) {
      if (!sym.isCold)
        res += Generic(s"The nested $sym should be marked as `@cold` in order to be instantiated", setting.pos)
    }
    else {  // field select
      res += Generic(s"The $sym may not be initialized", setting.pos)
    }

    res
  }

  /** assign to cold is always fine? */
  def assign(sym: Symbol, value: Value)(implicit setting: Setting): Res = Res()

  def init(constr: Symbol, values: List[Value], argPos: List[Position], obj: ObjectValue)(implicit setting: Setting): Res = {
    val paramInfos = constr.info.paramInfoss.flatten
    val res = Value.checkParams(constr.owner, paramInfos, values, argPos)
    if (res.hasErrors) return res

    val cls = constr.owner.asClass
    if (!cls.isCold) {
      res += Generic(s"The nested $cls should be marked as `@cold` in order to be instantiated", setting.pos)
      res.value = HotValue
      return res
    }

    obj.add(cls, WarmValue)

    Res()
  }

  def show(implicit setting: ShowSetting): String = "Cold"

  override def toString = "blank value"
}

/** A cold value, where class/trait params are initialized, but body fields are not
 *
 */
object ColdValue extends OpaqueValue {
  def select(sym: Symbol, isStaticDispatch: Boolean)(implicit setting: Setting): Res = {
    // set state to Hot, don't report same error message again
    val res = Res(value = HotValue)

    if (sym.is(Flags.Method)) {
      if (!sym.isCold && !sym.name.is(DefaultGetterName))
        res += Generic(s"The $sym should be marked as `@cold` in order to be called", setting.pos)

      res.value = Value.defaultFunctionValue(sym)
    }
    else if (sym.is(Flags.Lazy)) {
      if (!sym.isCold)
        res += Generic(s"The lazy field $sym should be marked as `@cold` in order to be accessed", setting.pos)
    }
    else if (sym.isClass) {
      if (!sym.isCold)
        res += Generic(s"The nested $sym should be marked as `@cold` in order to be instantiated", setting.pos)
    }
    else {  // field select
      if (!sym.isClassParam)
        res += Generic(s"The $sym may not be initialized", setting.pos)
    }

    res
  }

  /** assign to cold is always fine? */
  def assign(sym: Symbol, value: Value)(implicit setting: Setting): Res = Res()

  def init(constr: Symbol, values: List[Value], argPos: List[Position], obj: ObjectValue)(implicit setting: Setting): Res = {
    val paramInfos = constr.info.paramInfoss.flatten
    val res = Value.checkParams(constr.owner, paramInfos, values, argPos)
    if (res.hasErrors) return res

    val cls = constr.owner.asClass
    if (!cls.isCold) {
      res += Generic(s"The nested $cls should be marked as `@cold` in order to be instantiated", setting.pos)
      res.value = HotValue
      return res
    }

    obj.add(cls, WarmValue)

    Res()
  }

  def show(implicit setting: ShowSetting): String = "Cold"

  override def toString = "cold value"
}

object WarmValue extends OpaqueValue {
  def select(sym: Symbol, isStaticDispatch: Boolean)(implicit setting: Setting): Res = {
    val res = Res()
    if (sym.is(Flags.Method)) {
      if (!sym.isCold && !sym.isEffectiveInit && !sym.name.is(DefaultGetterName))
        res += Generic(s"The $sym should be marked as `@init` in order to be called", setting.pos)

      res.value = Value.defaultFunctionValue(sym)
    }
    else if (sym.is(Flags.Lazy) && !sym.isEffectiveInit) {
      if (!sym.isCold && !sym.isWarm)
        res += Generic(s"The lazy field $sym should be marked as `@init` in order to be accessed", setting.pos)

      res.value = sym.info.value.join(sym.value)
    }
    else {
      res.value = sym.info.value.join(sym.value)
    }

    res
  }

  def assign(sym: Symbol, value: Value)(implicit setting: Setting): Res =
    if (value.widen < sym.info.value)
      Res(effects = Vector(Generic("Cannot assign an object of a lower state to a field of higher state", setting.pos)))
    else Res()

  def init(constr: Symbol, values: List[Value], argPos: List[Position], obj: ObjectValue)(implicit setting: Setting): Res = {
    val paramInfos = constr.info.paramInfoss.flatten
    val res = Value.checkParams(constr.owner, paramInfos, values, argPos)
    if (res.hasErrors) return res

    val cls = constr.owner.asClass
    if (!cls.isCold && !cls.isWarm) {
      res += Generic(s"The nested $cls should be marked as `@init` in order to be instantiated", setting.pos)
      res.value = HotValue
      return res
    }

    obj.add(cls, WarmValue)

    Res()
  }

  def show(implicit setting: ShowSetting): String = "Warm"

  override def toString = "warm value"
}

/** A function value or value of method select */
abstract class FunctionValue extends SingleValue { self =>
  def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res

  def select(sym: Symbol, isStaticDispatch: Boolean)(implicit setting: Setting): Res = sym.name match {
    case nme.apply | nme.lift => Res(value = this)
    case nme.compose =>
      val selectedFun = new FunctionValue() {
        def apply(fun: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = {
          val composedFun = new FunctionValue() {
            def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = {
              val arg = values(0)
              val applySym = defn.FunctionClass(1).typeRef.member(nme.apply).symbol
              val res1 = fun(0).select(applySym)
              val res2 = res1.value.apply(arg :: Nil, argPos)
              val res3 = self.apply(res2.value :: Nil, argPos)
              Res(value = res3.value, effects = res1.effects ++ res2.effects ++ res3.effects)
            }
          }
          Res(value = composedFun)
        }
      }
      Res(value = selectedFun)
    case nme.andThen =>
      val selectedFun = new FunctionValue() {
        def apply(fun: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = {
          val composedFun = new FunctionValue() {
            def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = {
              val arg = values(0)
              val res1 = self.apply(arg :: Nil, argPos)
              val applySym = defn.FunctionClass(1).typeRef.member(nme.apply).symbol
              val res2 = fun(0).select(applySym)
              val res3 = res2.value.apply(res2.value :: Nil, argPos)
              Res(value = res3.value, effects = res1.effects ++ res2.effects ++ res3.effects)
            }
          }
          Res(value = composedFun)
        }
      }
      Res(value = selectedFun)
    case nme.applyOrElse =>
      val selectedFun = new FunctionValue() {
        def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = {
          val arg = values(0)
          val fun = values(1)
          val res1 = self.apply(arg :: Nil, argPos)
          val applySym = defn.FunctionClass(1).typeRef.member(nme.apply).symbol
          val res2 = fun.select(applySym)
          val res3 = res2.value.apply(arg :: Nil, argPos)
          Res(value = res1.value.join(res3.value), effects = res1.effects ++ res2.effects ++ res3.effects)
        }
      }
      Res(value = selectedFun)
    case nme.runWith =>
      val selectedFun = new FunctionValue() {
        def apply(fun: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = {
          val composedFun = new FunctionValue() {
            def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = {
              val arg = values(0)
              val res1 = self.apply(arg :: Nil, argPos)
              val applySym = defn.FunctionClass(1).typeRef.member(nme.apply).symbol
              val res2 = fun(0).select(applySym)
              val res3 = res2.value.apply(res2.value :: Nil, argPos)
              Res(value = HotValue, effects = res1.effects ++ res2.effects ++ res3.effects)
            }
          }
          Res(value = composedFun)
        }
      }
      Res(value = selectedFun)
    case nme.orElse =>
      val selectedFun = new FunctionValue() {
        def apply(fun: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = {
          val composedFun = new FunctionValue() {
            def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = {
              val arg = values(0)
              val res1 = self.apply(arg :: Nil, argPos)
              val applySym = defn.FunctionClass(1).typeRef.member(nme.apply).symbol
              val res2 = fun(0).select(applySym)
              val res3 = res2.value.apply(arg :: Nil, argPos)
              Res(value = res1.value.join(res3.value), effects = res1.effects ++ res2.effects ++ res3.effects)
            }
          }
          Res(value = composedFun)
        }
      }
      Res(value = selectedFun)
    case _ =>
      HotValue.select(sym)
  }

  /** not supported */
  def assign(sym: Symbol, value: Value)(implicit setting: Setting): Res = ???
  def init(constr: Symbol, values: List[Value], argPos: List[Position], obj: ObjectValue)(implicit setting: Setting): Res = ???

  def show(implicit setting: ShowSetting): String = toString

  override def toString: String = "Function@" + hashCode

  def join(that: FunctionValue): FunctionValue =
    new FunctionValue {
      def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = {
        val setting2 = setting.freshHeap
        val res1 = self(values, argPos)
        val res2 = that(values, argPos)(setting2)
        setting.heap.join(setting2.heap)
        res1.join(res2)
      }
    }

}

/** A lazy value */
abstract class LazyValue extends SingleValue {
  // not supported
  def select(sym: Symbol, isStaticDispatch: Boolean)(implicit setting: Setting): Res = ???
  def assign(sym: Symbol, value: Value)(implicit setting: Setting): Res = ???
  def init(constr: Symbol, values: List[Value], argPos: List[Position], obj: ObjectValue)(implicit setting: Setting): Res = ???

  def show(implicit setting: ShowSetting): String = toString

  override def toString: String = "LazyValue@" + hashCode
}

/** A slice of an object */
class SliceValue(val id: Int) extends SingleValue {
  /** not supported, impossible to apply an object value */
  def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = ???

  def select(sym: Symbol, isStaticDispatch: Boolean)(implicit setting: Setting): Res = {
    val slice = setting.heap(id).asSlice
    val value = slice(sym)

    if (sym.is(Flags.Lazy)) {
      if (value.isInstanceOf[LazyValue]) {
        val res = value(Nil, Nil)
        slice(sym) = res.value
        res
      }
      else Res(value = value)
    }
    else if (sym.is(Flags.Method)) {
      if (sym.info.isParameterless) {       // parameter-less call
        value(Nil, Nil)
      }
      else Res(value = value)
    }
    else {
      if (value == NoValue) {
        if (sym.info.isInstanceOf[ConstantType]) Res()
        else Res(effects = Vector(Uninit(sym, setting.pos)))
      }
      else {
        Res(value = value)
      }
    }
  }

  def assign(sym: Symbol, value: Value)(implicit setting: Setting): Res = {
    val slice = setting.heap(id).asSlice
    slice(sym) = value
    Res()
  }

  def init(constr: Symbol, values: List[Value], argPos: List[Position], obj: ObjectValue)(implicit setting: Setting): Res = {
    val cls = constr.owner.asClass
    val slice = setting.heap(id).asSlice
    val tmpl = slice.classInfos(cls)
    setting.analyzer.init(constr, tmpl, values, argPos, obj)(setting.withEnv(slice.innerEnv))
  }

  override def hashCode = id

  override def equals(that: Any) = that match {
    case that: SliceValue => that.id == id
    case _ => false
  }

  def show(implicit setting: ShowSetting): String = setting.heap(id).asSlice.show(setting)
}

class ObjectValue(val tp: Type, val open: Boolean = false) extends SingleValue {
  /** slices of the object */
  private var _slices: Map[ClassSymbol, Value] = Map()
  def slices: Map[ClassSymbol, Value] = _slices

  private var _dynamicCalls: Set[Symbol] = Set.empty
  def dynamicCalls: Set[Symbol] = _dynamicCalls
  def clearDynamicCalls(): Unit = {
    _dynamicCalls = Set.empty
  }

  def add(cls: ClassSymbol, value: Value) = {
    if (slices.contains(cls)) {
      _slices = _slices.updated(cls, _slices(cls).join(value))
    }
    else _slices = _slices.updated(cls, value)
  }

  // handle dynamic dispatch
  private def resolve(sym: Symbol)(implicit ctx: Context): Symbol = {
    if (sym.isClass || sym.isConstructor || sym.isEffectivelyFinal) sym
    else {
      // the method may crash, see tests/pos/t7517.scala
      try sym.matchingMember(tp) catch { case _: Throwable => NoSymbol }
    }
  }

  /** not supported, impossible to apply an object value */
  def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = ???

  def select(sym: Symbol, isStaticDispatch: Boolean)(implicit setting: Setting): Res = {
    val target = if (isStaticDispatch) sym else resolve(sym)

    // select on self type
    if (!target.exists) {
      if (sym.owner.is(Flags.Trait))
        return ColdValue.select(sym)
      else
        return WarmValue.select(sym)
    }

    if (this.widen == HotValue) return HotValue.select(sym)

    val res = Res()

    // remember dynamic calls
    if (!isStaticDispatch && !target.isEffectivelyFinal) {
      if (setting.allowDynamic || target.isEffectiveInit)
        _dynamicCalls = _dynamicCalls + target
      else
        res += Generic(s"Dynamic call to $target found", setting.pos)
    }

    val cls = target.owner.asClass
    if (slices.contains(cls)) {
      slices(cls).select(target) ++ res.effects
    }
    else {
      // select on unknown super
      assert(target.isDefinedOn(tp))
      WarmValue.select(target) ++ res.effects
    }
  }

  def assign(sym: Symbol, value: Value)(implicit setting: Setting): Res = {
    val target = resolve(sym)

    // select on self type
    if (!target.exists) return ColdValue.assign(sym, value)

    val cls = target.owner.asClass
    if (slices.contains(cls)) {
      slices(cls).assign(target, value)
    }
    else {
      // select on unknown super
      assert(target.isDefinedOn(tp))
      WarmValue.assign(target, value)
    }
  }

  def init(constr: Symbol, values: List[Value], argPos: List[Position], obj: ObjectValue)(implicit setting: Setting): Res = {
    val cls = constr.owner.asClass
    val outerCls = cls.owner.asClass
    if (slices.contains(outerCls)) {
      slices(outerCls).init(constr, values, argPos, obj)
    }
    else {
      val value = if (cls.isDefinedOn(tp)) WarmValue else ColdValue
      value.init(constr, values, argPos, obj)
    }
  }

  def show(implicit setting: ShowSetting): String = {
    val body = slices.map { case (k, v) => "[" +k.show + "]" + setting.indent(v.show(setting)) }.mkString("\n")
    "Object {\n" + setting.indent(body) + "\n}"
  }

  def annotate(cls: ClassSymbol)(implicit ctx: Context) = {
    dynamicCalls.foreach { sym =>
      debug(s"$sym used during initialization of $cls")
      cls.addAnnotation(Annotation.Call(sym))
    }
    _dynamicCalls = Set.empty
  }
}