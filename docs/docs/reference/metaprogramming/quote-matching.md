---
layout: doc-page
title: "Quote Matching Spec"
---

## Evaluation sematics

Bellow we provide an informal description of the pattern evailatuion.
We use `'{..}` for expression, `'[..]` for types and `{{..}}` for patterns nested in expressions.
The semantics are defined as a list of reduction rules that are tried one by one until one matches.

Operations:
* `s =?= p` checks if a scrutinee `s` matches the pattern `p` while accumulating extracted parts of the code.
* `isColosedUnder(x1, .., xn)('{e})` returns true if and only if all the references in `e` to names defined in the patttern are contained in the set `{x1, ... xn}`.
* `lift(x1, .., xn)('{e})` returns `(y1, ..., yn) => [xi = $yi]'{e}` where `yi` is an `Expr` of the type of `xi`.
* `withEnv(x1 -> y1, ..., xn -> yn)(matching)` evaluates mathing recording that `xi` is equivalent to `yi`.
* `matched` denotes that the the match succedded and `matched('{e})` denotes that a matech succeded and extracts `'{e}`
* `&&&` matches if both sides match. Concatenates the extracted expressions of both sides.

```scala
'{ e } =?= '{ $x }  &&  typeOf('{e}) <:< typeOf('{$x}) && isColosedUnder()('{e})  ===>   matched('{e})
'{ e } =?= '{ $y(x1, ..., xn) }  &&  isColosedUnder(x1, ... xn)('{e})  ===>   matched(lift(x1, ..., xn)('{e}))

'{ lit } =?= '{ lit }   ===>   matched
'{ e: T } =?= '{ p: P }   ===>   '{e} =?= '{p} &&& '[T] =?= '[P]
'{ e } =?= '{ p: P }   ===>   '{e} =?= '{p}
'{ e.x } =?= '{ p.x }   ===>   '{e} =?= '{p}
'{ x } =?= '{ x }   ===>   matched
'{e0.f(e1, ..., en)} =?= '{p0.f(p1, ..., p2)}   ===>   '{e0} =?= '{p0} &&& '{e1} =?= '{p1} &&& ... %% '{en} =?= '{pn}
'{e.f[T1, ..., Tn]} =?= '{p.f[P1, ..., Pn]}   ===>   '{e} =?= '{p} &&& '[T1] =?= '{P1} &&& ... %% '[Tn] =?= '[Pn]

'{ if e0 then e1 else e2 } =?= '{ if p0 then p1 else p2 }   ===>  '{e0} =?= '{p0} &&& '{e1} =?= '{p1} &&& '{e2} =?= '{p2}
'{ while e0 do e1 } =?= '{ while p0 do p1 }   ===>  '{e0} =?= '{p0} &&& '{e1} =?= '{p1}

'{ e0 = e1 } =?= '{ p0 = p1 } && '{e0} =?= '{p0}   ===>   '{e1} =?= '{p1}
'{ new T } =?= '{ new T }   ===>   matched
'{ this } =?= '{ this }   ===>   matched
'{ e.super[T] } =?= '{ p.super[T] }   ===>   '{e} =?= '{p}
'{ Seq(e0, ..., en): _* } =?= '{ Seq(p0, ..., pn): _* }   ===>   '{e0} =?= '{p0} &&& ... &&& '{en} =?= '{pn}

'[T] =?= '[P] && T <:< P   ===>   matched
'[ T0[T1, ..., Tn] ] =?= '[ P0[P1, ..., Pn] ]   ===>   '[T0] =?= '[P0] &&& ... &&& '[Tn] =?= '[Pn]
'[T @annot] =?= '[P]   ===>   '[T] =?= '[P]
'[T] =?= '[P @annot]   ===>   '[T] =?= '[P]

'{ {e0; e1; ...; en}; em } =?= '{ {p0; p1; ...; pn}; pm }   ===>   '{ e0; {e1; ...; en; em} } =?= '{ p0; {p1; ...; pn; pm} }
'{ val x: T = e1; e2 } =?= '{ val y: P = p1; p2 }   ===>   withEnv(x -> y)('[T] =?= '[P] &&& '{e1} =?= '{p1} &&& '{e2} =?= '{p2})
'{ def x0(x1: T1, ..., xn: Tn): T0 = e1; e2 } =?= '{ def y0(y1: P1, ..., yn: Pn): P0 = p1; p2 }   ===>   withEnv(x0 -> y0, ..., xn -> yn)('[T0] =?= '[P0] &&& ... &&& '[Tn] =?= '[Pn] &&& '{e1} =?= '{p1} &&& '{e2} =?= '{p2})

'{ e1; e2 } =?= '{ p1; p2 }   ===>   '{e1} =?= '{p1} &&& '{e2} =?= '{p2}

'{ e0 match { case u1 => e1; ...; case un => en } } =?= '{ p0 match { case q1 => p1; ...; case qn => pn } }   ===>
'{e0} =?= '{p0} &&& ... &&& '{en} =?= '{pn} &&& '{{u1}} =?= '{{q1}} &&& ... &&& '{{un}} =?= '{{qn}}

'{ try e0 catch { case u1 => e1; ...; case un => en } finally ef } =?= '{ try p0 catch { case q1 => p1; ...; case qn => pn } finally pf }   ===>   '{e0} =?= '{p0} &&& ... &&& '{en} =?= '{pn} &&& '{{u1}} =?= '{{q1}} &&& ... &&& '{{un}} =?= '{{qn}} &&& '{ef} =?= '{pf}

'{{ _ }} =?= '{{ _ }}   ===>   matched
'{{ x @ e }} =?= '{{ y @ p }}   ===>   withEnv(x -> y)('{{e}} =?= '{{p}})
'{{ e0(e1, ..., en)(using i1, ..., im ) }} =?= '{{ p0(p1, ..., pn)(using q1, ..., 1m) }}   ===>   '{{e0}} =?= '{{p0}} &&& ... &&& '{{en}} =?= '{{pn}} &&& '{i1} =?= '{q1} &&& ... &&& '{im} =?= '{qm}
'{{ e1 | ... | en }} =?= '{{ p1 | ... | pn }}   ===>   '{{e1}} =?= '{{p1}} &&& ... &&& '{{en}} =?= '{{pn}}
'{{ e: T }} =?= '{{ p: U }}   ===>   '{{e}} =?= '{{p}} &&& '[T] =?= [U]
'{{ e: T }} =?= '{{ p: U }}   ===>   '{{e}} =?= '{{p}} &&& '[T] =?= [U]
'{{ `x` }} =?= '{{ `y` }}   ===>   matched
```
