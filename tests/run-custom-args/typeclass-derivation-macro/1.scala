import scala.tasty._
import scala.quoted.{Expr => E, _}
import scala.annotation.tailrec

type RE[X] = given Reflection => E[X]

// Standard library

sealed trait Mirror[MonoType] {
  type Label
  type ElemLabels
  type ElemTypes
}

object Mirror {
  trait Product[MonoType] extends Mirror[MonoType]
  trait Sum[MonoType] extends Mirror[MonoType]
}

// Third party library

trait SummonInstances[F[_], T] {
  def instances: List[Any]
}
object SummonInstances {
  implicit def caseNil[F[_]]: SummonInstances[F, Unit] =
    new SummonInstances[F, Unit] { def instances = Nil }

  implicit def caseCons[F[_], H, T](implicit h: F[H], t: SummonInstances[F, T]): SummonInstances[F, (H, T)] =
    new SummonInstances[F, (H, T)] { def instances = h :: t.instances }
}

trait StagedProductInstances[F[_], T] {
  def instances: List[Any]
  def accessorsE(value: E[T])(implicit r: Reflection): List[E[Any]]
  def constructorE(fields: List[E[Any]])(implicit r: Reflection): E[T]

  def foldLeft2E[Acc](x: E[T], y: E[T])(i: E[Acc])(f: [t] -> (E[Acc], F[t], E[t], E[t]) => E[Acc]): RE[Acc] =
    accessorsE(x).zip(accessorsE(y)).zip(instances).foldLeft(i) {
      case (acc, ((xn, yn), in)) => f(acc, in.asInstanceOf, xn, yn)
    }
}

object StagedProductInstances {
  implicit def derive[F[_], T, ET]
    (implicit
      m: Mirror.Product[T] { type ElemTypes = ET },
      t: Type[T],
      s: SummonInstances[F, ET]
    ): StagedProductInstances[F, T] = new StagedProductInstances[F, T] {
      def instances: List[Any] = s.instances

      def accessorsE(value: E[T])(implicit r: Reflection): List[E[Any]] = {
        import r._
        t.unseal.symbol.asClassDef.caseFields.map { field =>
          Select.unique(value.unseal, field.name).seal
        }
      }

      def constructorE(fields: List[E[Any]])(implicit r: Reflection): E[T] = {
        import r._
        val companion = t.unseal.tpe match {
          case Type.SymRef(sym, prefix)   => Type.TermRef(prefix, sym.name)
          case Type.TypeRef(name, prefix) => Type.TermRef(prefix, name)
        }
        Select.overloaded(Ident(companion), "apply", Nil, fields.map(_.unseal)).seal.cast[T]
      }
    }
}

trait StagedCoproductInstances[F[_], T] {
  def instances: List[Any]
  def typetestsE(value: E[T])(implicit r: Reflection): List[E[Boolean]]
  def castsE(value: E[T])(implicit r: Reflection): List[E[Any]]

  def fold2E[R: Type](x: E[T], y: E[T])(i: E[R])(f: [t] -> (F[t], E[t], E[t]) => E[R]): RE[R] =
    typetestsE(x).zip(castsE(x)).zip(typetestsE(y).zip(castsE(y))).zip(instances).foldLeft(i) {
      case (acc, (((tx, cx), (ty, cy)), in)) =>
        '{ if ($tx && $ty) ${ f(in.asInstanceOf, cx, cy) } else $acc }
    }
}

object StagedCoproductInstances {
  implicit def derive[F[_], T, ET]
    (implicit
      m: Mirror.Sum[T] { type ElemTypes = ET },
      t: Type[T],
      s: SummonInstances[F, ET],
      x: SummonInstances[Type, ET],
    ): StagedCoproductInstances[F, T] = new StagedCoproductInstances[F, T] {
      def instances: List[Any] = s.instances

      def typetestsE(value: E[T])(implicit r: Reflection): List[E[Boolean]] =
        x.instances.map { tpe: Type[_] =>
          '{ $value.isInstanceOf[$tpe] }
          // TODO min
        }

      def castsE(value: E[T])(implicit r: Reflection): List[E[Any]] =
        x.instances.map {
          case tpe: Type[_] =>
            implicit val itpe = tpe
            value
        }
    }
}

// Type class definitions

trait Eq[A] {
  def eqv(x: A, y: A): Boolean
}

object Eq {
  inline def derive[T](implicit inline e: => Eq0[T]): Eq[T] =
    new Eq[T] {
      def eqv(a: T, b: T): Boolean = eqStaged(a, b)
    }

  inline def eqStaged[T](f1: T, f2: T)(implicit inline e: => Eq0[T]): Boolean =
    ${ eqStagedImpl('f1, 'f2)(e) }

  def eqStagedImpl[T](f1: E[T], f2: E[T])(e: Eq0[T])(implicit r: Reflection): E[Boolean] =
    e.eqv(f1, f2)
}

trait Eq0[A] {
  def eqv(x: E[A], y: E[A]): RE[Boolean]
}

object Eq0 {
  inline def apply[A](implicit ea: Eq0[A]): Eq0[A] = ea

  implicit def eqUnit: Eq0[Unit] = new Eq0[Unit] {
    def eqv(x: E[Unit], y: E[Unit]): RE[Boolean] = '{ true }
  }

  implicit def eqBoolean: Eq0[Boolean] = new Eq0[Boolean] {
    def eqv(x: E[Boolean], y: E[Boolean]): RE[Boolean] = '{ $x == $y }
  }

  implicit def eqInt: Eq0[Int] = new Eq0[Int] {
    def eqv(x: E[Int], y: E[Int]): RE[Boolean] = '{ $x == $y }
  }

  implicit def eqString: Eq0[String] = new Eq0[String] {
    def eqv(x: E[String], y: E[String]): RE[Boolean] = '{ $x == $y }
  }

  implicit def eqGen[A](implicit inst: StagedProductInstances[Eq0, A]): Eq0[A] =
    new Eq0[A] {
      def eqv(x: E[A], y: E[A]): RE[Boolean] =
        inst.foldLeft2E(x, y)('{ true })(
          [t] -> (acc: E[Boolean], eqt: Eq0[t], t0: E[t], t1: E[t]) =>
            '{ $acc && ${ eqt.eqv(t0, t1) }}
        )
    }

  // implicit def eqGenC[A](implicit inst: => K0.CoproductInstances[Eq0, A]): Eq0[A] =
    // new Eq0[A] {
    //   def eqv(x: E[A], y: E[A]): E[Boolean] = inst.fold2(x, y)(false)(
    //     [t] -> (eqt: Eq0[t], t0: t, t1: t) => eqt.eqv(t0, t1)
    //   )
    // }

  // inline def derive[A](gen: K0.ProductGeneric[A]): Eq0[A] =
  //   eqGen(K0.mkProductInstances[Eq0, A](gen))

  // inline def derive[A](gen: K0.CoproductGeneric[A]): Eq0[A] =
  //   eqGenC(K0.mkCoproductInstances[Eq0, A](gen))
}

// ADTs

case class ISB(i: Int, s: String, b: Boolean)

object ISB extends Mirror.Product[ISB] {
  type Label = "ISB"
  type ElemTypes = (Int, (String, (Boolean, Unit)))
  type ElemLabels = ("i", ("s", ("b", Unit)))

  inline implicit def mirror: this.type = ISB
}
