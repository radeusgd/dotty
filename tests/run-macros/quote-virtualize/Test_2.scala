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

      def Bind[T, R](exp: Eval[T], body: Var[T] => Eval[R]): Eval[R] = () => {
        body.apply(Var(exp())).apply()
      }

      def If[T](exp: Eval[Boolean], thenp: Eval[T], elsep: Eval[T]): Eval[T] = () =>
        if (exp()) thenp()
        else elsep()

      def Combine[T1, T2](s1: Eval[T1], s2: Eval[T2]): Eval[T2] = () => {
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

    given Symantics[Int, Eval] = new EvalSemantics[Int]

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

    import scala.concurrent.{Promise}
    import scala.concurrent.ExecutionContext.Implicits.global

    type Async = [T] =>> ((T => Unit) => Unit)

    class AsyncSemantics[T] extends Symantics[Promise[T], Async] {
      val futureRes: Promise[T] = Promise[T]()
      val res: Var[T] = Var(0.asInstanceOf[T])

      def Method(body: Async[T]): Promise[T] = {

        body.apply((r: T) => {
          futureRes.future.onComplete {
            case Success(value) => res.value = value.asInstanceOf[T]
            case Failure(e) => ???
          }
        })

        futureRes
      }

      def Return(exp: Async[T]): Async[T] = ???

      def Bind[T, R](exp: Async[T], body: Var[T] => Async[R]): Async[R] = (cont: (R => Unit)) => {
        exp.get((f: Future[T]) => {
          f.andThen
        })
      }

      def If[T](exp: Async[Boolean], thenp: Async[T], elsep: Async[T]): Async[T] = ???

      def Combine[T1, T2](s1: Async[T1], s2: Async[T2]): Async[T2] = ???

      def While(exp: Async[Boolean], body: Async[Unit]): Async[Unit] = ???

      def Inject[T](value: T): Async[T] = ???

      def App[A, B](f: Async[A => B], arg: Async[A]): Async[B] = ???

      def Abs[A, B](f: A => B): Async[A => B] = {
        Inject(f)
      }

      def Gt(lhs: Async[Int], rhs: Async[Int]): Async[Boolean] = ???

      def Lt(lhs: Async[Int], rhs: Async[Int]): Async[Boolean] = ???

      def Plus(arg1: Async[Int], arg2: Async[Int]): Async[Int] = ???

      def Mul(arg1: Async[Int], arg2: Async[Int]): Async[Int] = ???

      def Assign[T](lhs:  Var[T], rhs: Async[T]): Async[Unit] = ???

      def DeRef[T](x: Var[T]): Async[T] = ???
    }

    given Symantics[Future[Int], Async] = new AsyncSemantics[Int]

    val test4: Future[Int] = virtualize {
      var x: Int = Async.await(Future(1))
      var y: Int = Async.await(Future(10))
      x + y
    }

    test4
    println()

    // TODOs
    // val y = 11
    val testXX = virtualize {
      var x: Int = 1
      x = 11 + x
      x
    }

    println(testXX)
    println()

    // val y = 11
    // val test4: Int => Int = virtualize { (in: Int) =>{
    //   var x: Int = 1
    //   x = in + x
    //   x
    // }

    // val y = 11
    // val test4: Int => Int = (in: Int) => virtualize {
    //   var x: Int = 1
    //   x = in + x
    //   x
    // }
  }
}
