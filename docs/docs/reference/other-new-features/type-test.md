---
layout: doc-page
title: "TypeTest"
---

TypeTest
--------

`TypeTest` provides a replacement for `ClassTag.unapply` where the type of the argument is generalized.
`TypeTest.unapply` will return `Some(x.asInstanceOf[Y])` if `x` conforms to `Y`, otherwise it returns `None`.

```scala
trait TypeTest[-S, T] extends Serializable {
  def unapply(s: S): Option[s.type & T]
}
```

Just like `ClassTag` used to do, it can be used to perform type checks in patterns.

```scala
type X
type Y <: X
given TypeTest[X, Y] = ...
(x: X) match {
  case y: Y => ... // safe checked downcast
  case _ => ...
}
```


Examples
--------

Given the following abstract definition of `Peano` numbers that provides `TypeTest[Nat, Zero]` and `TypeTest[Nat, Succ]`

```scala
trait Peano {
  type Nat
  type Zero <: Nat
  type Succ <: Nat
  def safeDiv(m: Nat, n: Succ): (Nat, Nat)
  val Zero: Zero
  val Succ: SuccExtractor
  trait SuccExtractor {
    def apply(nat: Nat): Succ
    def unapply(nat: Succ): Option[Nat]
  }
  given TypeTest[Nat, Zero] = typeTestOfZero
  protected def typeTestOfZero: TypeTest[Nat, Zero]
  given TypeTest[Nat, Succ]
  protected def typeTestOfSucc: TypeTest[Nat, Succ]
```

it will be possible to write the following program

```scala
val peano: Peano = ...
import peano.{_, given}
def divOpt(m: Nat, n: Nat): Option[(Nat, Nat)] = {
  n match {
    case Zero => None
    case s @ Succ(_) => Some(safeDiv(m, s))
  }
}
val two = Succ(Succ(Zero))
val five = Succ(Succ(Succ(two)))
println(divOpt(five, two))
```

Note that without the `TypeTest[Nat, Succ]` the pattern `Succ.unapply(nat: Succ)` would be unchecked.