
package a {

  import scala.compiletime.S

  trait QCtx { qctx =>
    type Level <: Int
    type Next = S[Level]
    type Pred = Level match { case S[l] => l }
  }
  type Expr[+T]

  /*Quote*/ def q[L <: Int, T](using qctx: QCtx { type Level = L })(x: T): Expr[T] = ???
  /*Splice*/ def s[L <: Int, T](x: QCtx { type Level = L } ?=> Expr[T]): T = ???
  /*run*/ def r[T](x: (qctx: QCtx { type Level = 0 }) ?=> Expr[T]): T = ???

  val test: Any = {

    def pow(using QCtx)(x: Expr[Double], n: Int): Expr[Double] =
      if n == 0 then q{1.0} else q{ s{x} * s{pow(x, n - 1)} }

    def pow2(using QCtx)(x: Expr[Double], n: Int): Expr[Double] =
      if n == 0 then q{1.0} else q{ s{x} * s{pow(x, n - 1)} }

    r {
      q[L=0]{ (x: Double) => s[L=0]{pow(q[L=0]{x}, 5)} }
    }

    r {
      q[L=0]{ (x: Double) =>
        s[L=0]{
          val y = q[L=0]{x}
          pow(q[L=0]{s[L=0]{y}}, 5)
        }
      }
    }

    r {
      val a = q[L=0]{ 4.0 }
      q[L=0]{ (x: Double) =>
        s[L=0]{
          pow(q[L=0]{s[L=0]{a}}, 5)
        }
      }
    }

    r {
      val a = q[L=0]{ 4.0 }
      q[L=0]{ (x: Double) => (qctx1: QCtx { type Level = 1 }) ?=>
        q[L=1]{
          s[L=1]{
            pow(q[L=1]{s[L=1]{a}}, 5)
            s[L=0]{ q[L=0]{ q[L=1]{ 3 } } }
          }
        }
      }
    }

    r {
      val a = q[L=0]{ 4.0 }
      q[L=0]{ (x: Double) => (qctx1: QCtx { type Level = 1 }) ?=>
        q[L=1]{
          s[L=1]{
            pow(q[L=1]{s[L=1]{a}}, 5)
            s[L=0]{ q[L=1]{ q[L=1]{ 3 } } }
          }
        }
      }
    }

    // r {
    //   val a = q[L=0]{ 4.0 }
    //   q[L=0]{ (x: Double) => // error(qctx1: QCtx { type Level = 1 }) ?=>
    //     q[L=1]{ // error: no implicit argument of type a.QCtx{Level = (1 : Int)} was found
    //       s[L=1]{
    //         pow(q[L=1]{s[L=1]{a}}, 5)
    //         s[L=0]{ q[L=0]{ q[L=1]{ 3 } } }
    //       }
    //     }
    //   }
    // }


  }

}
