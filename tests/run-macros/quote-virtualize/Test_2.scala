import scala.quoted._
import scala.quoted.matching._
import Macros._

object Test {
  def main(args: Array[String]): Unit = {

    type Eval = [T] =>> () => T

    class EvalSemantics[T] extends Symantics[T, Eval] {
      val result : Var[T] = Var(0.asInstanceOf[T])

      def Method(body: Eval[T]): T = {
        body.apply()

        // result.value
      }

      def Return(exp: Eval[T]): Eval[T] = () => {
        result.update(exp())
        result.value
      }

      def ValDef[T, R](exp: Eval[T], body: Var[T] => Eval[R]): Eval[R] = () => {
        body.apply(Var(exp())).apply()
      }

      def If[T](exp: Eval[Boolean], thenp: Eval[T], elsep: Eval[T]): Eval[T] = () =>
        if (exp()) thenp()
        else elsep()

      def Block[T1, T2](s1: Eval[T1], s2: Eval[T2]): Eval[T2] = () => {
        s1()
        s2()
      }

      def While(exp: Eval[Boolean], body: Eval[Unit]): Eval[Unit] = () => {
        while(exp()){
          body.apply()
        }
      }

      def Inject[T](value: T): Eval[T] = () => {
        value
      }

      def App[A, B](f: Eval[A => B], arg: Eval[A]): Eval[B] = () => {
        f.apply.apply(arg.apply)
      }

      def Abs[A, B](f: A => B): Eval[A => B] = {
        Inject(f)
      }

      def Gt(lhs: Eval[Int], rhs: Eval[Int]): Eval[Boolean] = () => {
        lhs() > rhs()
      }

      def Lt(lhs: Eval[Int], rhs: Eval[Int]): Eval[Boolean] = () => {
        lhs() < rhs()
      }

      def Plus(arg1: Eval[Int], arg2: Eval[Int]): Eval[Int] = () => {
        arg1() + arg2()
      }

      def Mul(arg1: Eval[Int], arg2: Eval[Int]): Eval[Int] = () => {
        arg1() * arg2()
      }

      def Assign[T](lhs:  Var[T], rhs: Eval[T]): Eval[Unit] = () => {
        val ret = rhs()
        lhs.update(ret)
      }

      def DeRef[T](x: Var[T]): Eval[T] = () => {
        x.value
      }
    }

    type Cont = [T, Err <: Throwable] =>> (T, Err) => Unit

    given Symantics[Int, Eval] = new EvalSemantics[Int]
    // given Symantics[Future[Int], Cont] = new EvalSemantics[Int]

    val test00 = virtualize {
      1
    }

    println(test00)
    println()

    val test01 = virtualize {
      var x: Int = 42
      x
    }

    println(test01)
    println()

    val test02 = virtualize {
      var x: Int = 1
      var y: Int = 2
      x + y
    }

    println(test02)
    println()

    val test031 = virtualize {
      var x: Int = 1
      x = x + 1
      x
    }

    println(test031)
    println()

    val test03: (given Symantics[Int, Eval]) => Int = virtualize {
      var x = 1
      if (x > 0)
        x = x * 2
      else
        x = 0
      x
    }

    println(test03)
    println()

    val test013 = virtualize {
      var x: Int = 1
      println(x)
      x
    }

    test013
    println()

    val test3 = virtualize {
      var x: Int = 0
      while (x < 5) {
        println(x)
        x = x + 1
      }
      x
    }

    test3
    println()

    // TODO
    /*
        val test03: (given Symantics[Int, Eval]) => Int => Int = virtualize { (in: Int) => ...
        or
        val test03: (given Symantics[Int, Eval]) => Int => Int = (in: Int) => virtualize {
    */
  }
}
