import scala.compiletime._

object Test {
  type Id[t] = t
  type Const[c] = [t] => c

  class Da
  class Db
  class Dummy // = Da & Db

  case class Wrap[T](t: T)

  type Apply[T[_]] = T[Dummy]
  type Unapply[F[_[_]], T] = T match {
    case Wrap[Apply[a]] => F[a]
    case Wrap[Dummy] => F[Id]
    case Wrap[c] => F[Const[c]]
  }

  inline def summon[F[_[_]], T] = implicit match {
    case ft: Unapply[F, Wrap[T]] => ft
  }

  inline def summonAsArray[F[_[_]], T[_]]: Array[Any] = inline erasedValue[Apply[T]] match {
    case _: Unit => Array()
    case _: Tuple1[a] => Array(summon[F, a])
    case _: (a, b) => Array(summon[F, a], summon[F, b])
    //case _: (a, b, c) => Array(summon[a], summon[b], summon[c])
    //case _: (a, b, c, d) => Array(summon[a], summon[b], summon[c], summon[d])
    //case _: (a, b, c, d, e) => Array(summon[a], summon[b], summon[c], summon[d], summon[e])
    // Add fallback for larger sizes
  }

  trait Opt[+A]
  class Sm[+A] extends Opt[A]
  case object Nn extends Opt[Nothing]
  type ElemTypes = [t] => (Sm[t], Nn.type)
  //type ElemTypes = [t] => Tuple1[Sm[t]]
  //type ElemTypes = [t] => Tuple1[Nn.type]

  trait Functor[F[_]]
  object Functor {
    implicit def functorConst[T]: Functor[Const[T]] = ???
    implicit def functorSm: Functor[Sm] = ???
  }

  val v0 = summonAsArray[Functor, ElemTypes]

  type Lub[F[_[_]], T[_]] = Apply[T] match {
    case (a *: b) => Lub0[F, b, Unapply[F, Wrap[a]]]
  }

  type Lub0[F[_[_]], T, L] = T match {
    case Unit => L
    case (a *: b) => Lub0[F, b, Unapply[F, Wrap[a]] | L]
  }

  inline def summonFirst[F[_[_]], T[_]] <: Any = summonFirst0[F, Apply[T]]

  inline def summonFirst0[F[_[_]], T] <: Any = inline erasedValue[T] match {
    case _: (a *: b) => summonFirst1[F, Unapply[F, Wrap[a]], b]
  }

  inline def summonFirst1[F[_[_]], FH, T] <: Any = implicit match {
    case fh: FH => fh
    case _ => summonFirst0[F, T]
  }

  trait EmptyK[F[_]] {
    def empty[A]: F[A]
  }
  object EmptyK {
    implicit def emptyKConst[T]: EmptyK[Const[T]] = ???
    implicit def emptyKSm: EmptyK[Sm] = ???
  }

  type ElemTypes2 = [t] => (Sm[t], Nn.type)
  //type ElemTypes2 = [t] => Tuple1[Sm[t]]
  //type ElemTypes2 = [t] => Tuple1[Nn.type]

  val v1 = summonFirst[EmptyK, ElemTypes2]

  val v2 = v1.empty[Int]

  def blah[T]: Opt[T] = v1.empty[T]

  val v3 = blah[Int]

  //implicitly[Lub[EmptyK, ElemTypes] =:= (EmptyK[Sm] | EmptyK[Const[Nn.type]])]
  //val v2: Lub[Functor, ElemTypes] = null.asInstanceOf[Functor[Sm]]

  val v4: EmptyK[Sm] = ???
  val v5: EmptyK[Const[Nn.type]] = ???
  //val v6: (EmptyK[Sm] | EmptyK[Const[Nn.type]]) = ???
  val v6: (EmptyK[Sm]) = ???
  val v7 = v6.empty[Int]
}


/*
object Test {
  type Const[c] = [t] => c
  type ConstP[c] = [+t] => c

  inline def summon[T] =
    implicit match {
      case t: T => t
    }

  class A[+T]
  object B

  type FA = A
  type FB = [+t] => B.type

  type D[_[_]]
  implicit val da: D[A] = ???
  //implicit val dc: D[FB] = ???
  //implicit val dc: D[ConstP[B.type]] = ???
  implicit val dc: D[Const[B.type]] = ???

  val v0 = implicitly[D[FA]]
  //val v1 = implicitly[D[FB]]

  //implicitly[FB[Any] =:= ConstP[B.type][Any]]
  //implicitly[D[FB] =:= D[ConstP[B.type]]]

  
}

object Test2 {
  {
    class Co[+T]
    class Inv[T]
    class Con[-T]

    trait F[T[_]]
    implicit def f[T[_]]: F[T] = ???

    implicitly[F[Co]]
    implicitly[F[Inv]]
    implicitly[F[Con]]
  }

  {
    type Co[+T] = Any
    type Inv[T] = Any
    type Con[-T] = Any

    trait F[T[_]]
    implicit def f[T[_]]: F[T] = ???

    implicitly[F[Co]]
    implicitly[F[Inv]]
    implicitly[F[Con]]
  }

  {
    type Co = [+T] => Any
    type Inv = [T] => Any
    type Con = [-T] => Any

    trait F[T[_]]
    implicit def f[T[_]]: F[T] = ???

    implicitly[F[Co]]
    implicitly[F[Inv]]
    implicitly[F[Con]]
  }

  {
    type Co[C] = [+T] => C
    type Inv[C] = [T] => C
    type Con[C] = [-T] => C

    trait F[T[_]]
    implicit def f[T[_]]: F[T] = ???

    implicitly[F[Co[Int]]]
    implicitly[F[Inv[Int]]]
    implicitly[F[Con[Int]]]
  }

  {
    object S

    type Co[C] = [+T] => C
    type Inv[C] = [T] => C
    type Con[C] = [-T] => C

    trait F[T[_]]
    implicit def f[T[_]]: F[T] = ???

    implicitly[F[Co[S.type]]]
    implicitly[F[Inv[S.type]]]
    implicitly[F[Con[S.type]]]
  }

  {
    object S

    type Co[C] = [+T] => C
    type Inv[C] = [T] => C
    type Con[C] = [-T] => C

    trait F[T[_]]

    {
      implicit def f[T]: F[Co[T]] = ???

      implicitly[F[Co[S.type]]]
      //implicitly[F[Inv[S.type]]]
      //implicitly[F[Con[S.type]]]
    }

    {
      implicit def f[T]: F[Inv[T]] = ???

      //implicitly[F[Co[S.type]]]
      implicitly[F[Inv[S.type]]]
      //implicitly[F[Con[S.type]]]
    }

    {
      implicit def f[T]: F[Con[T]] = ???

      //implicitly[F[Co[S.type]]]
      //implicitly[F[Inv[S.type]]]
      implicitly[F[Con[S.type]]]
    }
  }

  {
    class S

    type Co[C] = [+T] => C
    type Inv[C] = [T] => C
    type Con[C] = [-T] => C

    trait F[T[_]]

    {
      implicit def f[T]: F[Co[T]] = ???

      implicitly[F[Co[S]]]
      //implicitly[F[Inv[S]]]
      //implicitly[F[Con[S]]]
    }

    {
      implicit def f[T]: F[Inv[T]] = ???

      //implicitly[F[Co[S]]]
      implicitly[F[Inv[S]]]
      //implicitly[F[Con[S]]]
    }

    {
      implicit def f[T]: F[Con[T]] = ???

      //implicitly[F[Co[S]]]
      //implicitly[F[Inv[S]]]
      implicitly[F[Con[S]]]
    }
  }
}
*/
