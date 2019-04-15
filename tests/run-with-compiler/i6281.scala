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
    def reify[A] given Type[A]   = m1 => '{ k1 => ${ '{ k => ${ (m1(a1 => k2 => ('k1(a1)).apply('{ a => ${  k2('a) } }))).apply(a => 'k(a)) }}}}
    def reflect[A] given Type[A] = m =>    k =>    k3 => m('{ a => ${ '{ k2 => ${ k('a)(a => 'k2(a)) }} } })('{ a2 => ${  k3('a2) } })
  }

  implied cons [L <: HList] given Effects[L] given Type[L] for Effects[Int :: L] {
    def reify[A] given Type[A]   = m => '{ k => ${ Effects[L].reify[Int] {   m(   a =>    Effects[L].reflect[Int]('k(a))) } }}
    def reflect[A] given Type[A] = m =>    k =>    Effects[L].reflect[Int] { m('{ a => ${ Effects[L].reify[Int](   k('a)) } })}
  }

  def Effects[L <: HList] given Effects[L]: Effects[L] = the[Effects[L]]

  def main(args: Array[String]): Unit = {
    val m: STM[Int, Int :: Int :: Int :: HNil] = k => k('{42})

    val effects = Test.cons[Int :: Int :: HNil](Test.cons1)('[Int :: Int :: HNil])

    println(effects.reify[Int] { m }.show)
  }
}