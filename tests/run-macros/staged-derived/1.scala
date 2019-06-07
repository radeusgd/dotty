import scala.quoted.{Type, Expr => E}

trait Eq[A] {
  def e(a: A, b: A): Boolean
}

trait Eq0[A] {
  def e(a: A, b: A): E[Boolean]
}

trait Generic[T] {
  type Repr
  def to(x: E[T]): Repr
  def from(x: Repr): E[T]
}

sealed trait Sum
case class CCons[X: Type, Y <: Sum](value: E[Any], tail: Y) extends Sum {
  def isHead: E[Boolean] = '{ $value.isInstanceOf[X] }
  def asHead: E[X] = as[X]
  def as[Z: Type]: E[Z] = '{ $value.asInstanceOf[Z] }
}
trait CNil extends Sum
case object CNil extends CNil

object Eq0 {
  implicit def eqString: Eq0[E[String]] = new Eq0[E[String]] {
    def e(a: E[String], b: E[String]) =
      '{ $a == $b }
  }

  implicit def eqInt: Eq0[E[Int]] = new Eq0[E[Int]] {
    def e(a: E[Int], b: E[Int]) =
      '{ $a == $b }
  }

  implicit def caseUnit: Eq0[Unit] = new Eq0[Unit] {
    def e(a: Unit, b: Unit) =
      '{ true }
  }

  implicit def caseCNil: Eq0[CNil] = new Eq0[CNil] {
    def e(a: CNil, b: CNil): E[Boolean] =
      '{ false } // ???
  }

  implicit def caseGen[A, R](implicit g: Generic[A] { type Repr = R }, eqr: Eq0[R]): Eq0[E[A]] = new Eq0[E[A]] {
    def e(a: E[A], b: E[A]): E[Boolean] =
      eqr.e(g.to(a), g.to(b))
  }

  implicit def caseProd[X, Y](implicit sx: Eq0[X], sy: Eq0[Y]): Eq0[(X, Y)] = new Eq0[(X, Y)] {
    def e(p: (X, Y), q: (X, Y)): E[Boolean] =
      '{ ${ sx.e(p._1, q._1) } && ${ sy.e(p._2, q._2) }}
  }

  implicit def caseCCons[X, Y <: Sum](implicit sx: Eq0[E[X]], sy: Eq0[Y]): Eq0[CCons[X, Y]] = new Eq0[CCons[X, Y]] {
    def e(p: CCons[X, Y], q: CCons[X, Y]): E[Boolean] =
      '{
        ${ p.isHead } == ${ q.isHead } &&
        (${ p.isHead } && ${ sx.e(p.asHead, q.asHead) }) || ${ sy.e(p.tail, q.tail) }
      }
  }
}

case class Foo(i: Int, s: String)

object Foo {
  implicit def genFoo: Generic[Foo] { type Repr = (E[Int], (E[String], Unit)) } =
    new Generic[Foo] {
      type Repr = (E[Int], (E[String], Unit))
      def to(c: E[Foo]): Repr   =
        ('{ $c.i }, ('{ $c.s }, ()))

      def from(r: Repr): E[Foo] =
        '{ Foo(${ r._1 }, ${ r._2._1 }) }
    }
}

sealed trait Bar
case class Bur(i: Int) extends Bar
case class Bor(s: String) extends Bar

def eql(a: Bar, b: Bar): Boolean = {
  (a, b) match {
    case (a: Bur, b: Bur) => a.i == b.i
    case (a: Bor, b: Bor) => a.s == b.s
    case _ => false
  }
}


object Bur {
  implicit def genBur: Generic[Bur] { type Repr = (E[Int], Unit) } =
    new Generic[Bur] {
      type Repr = (E[Int], Unit)
      def to(c: E[Bur]): Repr   =
        ('{ $c.i }, ())

      def from(r: Repr): E[Bur] =
        '{ Bur(${ r._1 }) }
    }
}

object Bor {
  implicit def genBor: Generic[Bor] { type Repr = (E[String], Unit) } =
    new Generic[Bor] {
      type Repr = (E[String], Unit)
      def to(c: E[Bor]): Repr   =
        ('{ $c.s }, ())

      def from(r: Repr): E[Bor] =
        '{ Bor(${ r._1 }) }
    }
}

object Bar {
  implicit def genBar: Generic[Bar] { type Repr = CCons[Bur, CCons[Bor, CNil]] } =
    new Generic[Bar] {
      type Repr = CCons[Bur, CCons[Bor, CNil]]
      def to(c: E[Bar]): Repr =
        CCons(c, CCons(c, CNil))
      def from(r: Repr): E[Bar] = r.as[Bar]
    }
}

case class Fab(a: Int, b: String)


object Eq0Macro {
  inline def derived[T](implicit inline e: Eq0[E[T]]): Eq[T] =
    new Eq[T] {
      def e(a: T, b: T): Boolean = eqStaged(a, b)
    }

  inline def eqStaged[T](f1: T, f2: T)(implicit inline e: Eq0[E[T]]): Boolean =
    ${ eqStagedImpl('{ f1 }, '{ f2 })(e) }

  def eqStagedImpl[T](f1: E[T], f2: E[T])(e: Eq0[E[T]]): E[Boolean] =
    e.e(f1, f2)
}
