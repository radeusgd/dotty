import scala.quoted._

object Test {

  val toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
  implied for scala.quoted.Toolbox = toolbox

  sealed trait HList
  sealed trait HNil extends HList
  sealed trait ::[E, T <: HList] extends HList

  type STM[A, L <: HList] = L match {
    case HNil => Expr[A]
    case e :: rs => (Expr[A] => STM[e, rs]) => STM[e, rs]
  }

  type Stm[A, L <: HList] = L match {
    case HNil => A
    case e :: rs => (A => Stm[e, rs]) => Stm[e, rs]
  }

  trait Effects[L <: HList] {
    def reify[A] given Type[A]: STM[A, L] => Expr[Stm[A, L]]
    def reflect[A] given Type[A]: Expr[Stm[A, L]] => STM[A, L]
  }

  implied cons1 for Effects[Int :: Int :: HNil] {
    def reify[A] given Type[A]   = m1 => '{ (k1: A => (Int => Int) => Int) => (k3: Int => Int) => ${ (m1.apply(a1 => (k2: Expr[Int] => Expr[Int]) => ('k1(a1)).apply('{ a => ${  k2('a) } }))).apply((a: Expr[Int]) => 'k3(a))}}
    def reflect[A] given Type[A] = m1 => (k1:quoted.Expr[A] => (quoted.Expr[Int] => quoted.Expr[Int]) => quoted.Expr[Int]) => (k3: Expr[Int] => Expr[Int]) => m1.apply('{ a: A => (k2: Int => Int) => ${ k1('a).apply(a => 'k2(a)) }}).apply('{ (a2: Int) => ${ k3('a2)}})
  }

  implied cons2 [L <: HList] given Effects[Int ::  Int :: HNil] given Type[L] for Effects[Int :: L] {
    def reify[A] given Type[A]   = m => '{ k =>'{ (k19: (Int => Test.Stm[Int, Test.HNil]) => Test.Stm[Int, Test.HNil]) => (k3: Int => Int) => ${ ((m(a10 =>  (k1: quoted.Expr[A] => (quoted.Expr[Int] => quoted.Expr[Int]) => quoted.Expr[Int]) => (k3: Expr[Int] => Expr[Int]) => ('k(a10)).apply('{ a18: A => (k2: Int => Int) => ${ k1('a18).apply(a19 => 'k2(a19)) }}).apply('{ a2 => ${ k3('a2)}})) ).apply(a1 => (k2: Expr[Int] => Expr[Int]) => ('k19(a1)).apply('{ a => ${  k2('a) } }))).apply(a => 'k3(a))}}}
    def reflect[A] given Type[A] = m =>   (k: quoted.Expr[A] => Test.STM[Int, L]) => (k45:quoted.Expr[A] => (quoted.Expr[Int] => quoted.Expr[Int]) => quoted.Expr[Int]) => (k73: Expr[Int] => Expr[Int]) => (m('{ a: A => ${ (k1:quoted.Expr[A] => (quoted.Expr[Int] => quoted.Expr[Int]) => quoted.Expr[Int]) => (k3: Expr[Int] => Expr[Int]) => (k('a)).apply('{ a: A => (k2: Int => Int) => ${ k1('a).apply(a => 'k2(a)) }}).apply('{ (a2: Int) => ${ k3('a2)}}) } })).apply('{ a44: A => (k32: Int => Int) => ${ k45('a44).apply(a => 'k32(a)) }}).apply('{ (a32: Int) => ${ k73('a32)}})
  }

  def Effects[L <: HList] given Effects[L]: Effects[L] = the[Effects[L]]

  def main(args: Array[String]): Unit = {
    val m: STM[Int, Int :: Int :: Int :: HNil] = k => k('{42})

    val effects = Test.cons2[Int :: Int :: HNil](Test.cons1)('[Int :: Int :: HNil])

    println(effects.reify[Int] { m }.show)
  }
}