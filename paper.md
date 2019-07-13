Staged typeclass derivation


# Introduction

Typeclass derivation is convenient but slow

Contributions:

- A new typeclass derivation framework
- A new staging technique that is entirely type-driven, making use of Scala's implicits
- Evaluation of runtime and compile time performance

# Typeclass derivation

- traditional sum of product representation
- the mirror interface
- implicit search îs used to gather instances

    {
      class SummonInstances[F[_], T](val instances: List[Any])
      object SummonInstances {
        implicit def caseNil[F[_]]: SummonInstances[F, Unit] =
          new SummonInstances[F, Unit](Nil)

        implicit def caseCons[F[_], H, T <: Tuple](implicit h: F[H], t: SummonInstances[F, T]): SummonInstances[F, H *: T] =
          new SummonInstances[F, H *: T](h :: t.instances)
      }
    }

- dotty's derives clause for algebraic datatypes (sealed traits and case classes). Show the desugaring with an example.

# Compile-time staging

- Two layers of typeclasses, Eq/Eq0

    implicit def eqInt: Eq0[Int] = new Eq0[Int] {
      def eqv(x: E[Int], y: E[Int]): RE[Boolean] =
        '{ $x == $y }
    }

    trait StagedProductInstances[F[_], T] {
      def instances: List[Any]
      def accessorsE(value: E[T]): List[E[Any]]
      def constructorE(fields: List[E[Any]]): E[T]

      def foldLeft2E[R](x: E[T], y: E[T])(i: E[R])(f: [t] => (E[R], F[t], E[t], E[t]) => E[R]): E[R] =
        accessorsE(x).zip(accessorsE(y)).zip(instances).foldLeft(i) {
          case (acc, ((xn, yn), in)) => f(acc, in, xn, yn)
        }
    }

    trait StagedCoproductInstances[F[_], T] {
      def instances: List[Any]
      def typetestsE(value: E[T]): List[E[Boolean]]
      def castsE(value: E[T]): List[E[Any]]

      def fold2E[R](x: E[T], y: E[T])(i: E[R])(f: [t] => (F[t], E[t], E[t]) => E[R]): E[R] =
        typetestsE(x).zip(castsE(x)).zip(typetestsE(y).zip(castsE(y))).zip(instances).foldLeft(i) {
          case (acc, (((tx, cx), (ty, cy)), in)) =>
            '{ if ($tx && $ty) ${ f(in, cx, cy) } else $acc }
        }
    }


# Evaluation

Comparing

- shapeless2
- hand written
- shapeless3
- shapeless3-staged

Using 3 typeclasses

- Eq
- Functor
- Codec


# Related work

- GHC generic
- Optimizing generic programing thought inlining (rewrite rules)


# Conclusion
