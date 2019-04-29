
import scala.quoted._
import scala.quoted.matching._

import scala.tasty.Reflection
import scala.quoted.matching._

object Macros {

  inline def virtualize[Ret, Exp[_]](a: => Ret)(given sym: Symantics[Ret, Exp]) : Ret = ${  virtualizeImpl[Ret, Exp]('{a}, '{sym}) }

  private def virtualizeImpl[Ret: Type, Exp[_]: Type](a: Expr[Ret], sym: Expr[Symantics[Ret, Exp]])(given qctx: QuoteContext): Expr[Ret] = {
    import qctx.tasty.{_, given}

    // Env Utilities
    type Env = Map[Sym[_], Expr[Var[_]]]

    // def liftNumeric[T: quoted.Type](e: Expr[T]) given (num: scala.math.Numeric[T]): Expr[Nothing] = {
    //   import num._

    //   e match {
    //     case '{ ($x: T) + ($y: T) } => '{ ??? }
    //   }
    // }

    object Unseal {
      def unapply[T](e: Expr[T])(given qctx: QuoteContext) = {
        import qctx.tasty._
        Some(e.unseal)
      }
    }

    def lift[T: quoted.Type](e: Expr[T], last: Boolean)(env: Env): Expr[Exp[T]] = {

      e match {
        case '{ var $y_bind: $t = $z; $k:T } => '{
            // TODO: symbol table insert
            $sym.ValDef[$t, T](${lift(z, false)(env)}, (y: Var[$t]) => ${lift(k, last)(env + (y_bind -> 'y))})
        }.cast[Exp[T]]

        case '{ (${ Unseal(Assign(lhs, rhs)) }): $t } =>
          val ret = env(new Sym(lhs.symbol.name, lhs.symbol))

          type TT
          implicit val ttype: quoted.Type[TT] = rhs.tpe.widen.seal.asInstanceOf[quoted.Type[TT]]
          // given quoted.Type[TT] = rhs.tpe.widen.seal.asInstanceOf[quoted.Type[TT]] (crashed)

          '{
            $sym.Assign[TT](
              ${ret.cast[Var[TT]]},
              ${lift(rhs.seal.cast[TT], false)(env)})
          }.cast[Exp[T]]

        case '{ (if ($cond) $thenp else $elsep): $t } => '{
          $sym.If[$t](${lift(cond, false)(env)}, ${lift(thenp, false)(env)}, ${lift(elsep, false)(env)})
        }.cast[Exp[T]]

        case '{ (while($cond) $body) } => '{
          $sym.While(${lift(cond, false)(env)}, ${lift(body, false)(env)})
        }.cast[Exp[T]]

        case '{ ($stmt1: $t); $stmt2 } =>
          type TT = $t
          '{
            $sym.Block(${lift[TT](stmt1, true)(env)}, ${lift(stmt2, true)(env)})
          }.cast[Exp[T]]

        case e if last => '{
          $sym.Return(${lift(e, false)(env).cast[Exp[Ret]]} )
        }.cast[Exp[T]]

        case '{( $x: Int) + ($y: Int) } => '{
          $sym.Plus(${lift(x, false)(env)}, ${lift(y, false)(env)})
        }.cast[Exp[T]]

        case '{( $x: Int) > ($y: Int) } => '{
          $sym.Gt(${lift(x, false)(env)}, ${lift(y, false)(env)})
        }.cast[Exp[T]]

        case '{( $x: Int) < ($y: Int) } => '{
          $sym.Lt(${lift(x, false)(env)}, ${lift(y, false)(env)})
        }.cast[Exp[T]]

        case '{( $x: Int) * ($y: Int) } => '{
          $sym.Mul(${lift(x, false)(env)}, ${lift(y, false)(env)})
        }.cast[Exp[T]]

        // scala.Predef.println(x)
        // ->
        // () => scala.Predef.println(x.value)
        case '{ (${ Unseal(Apply(f, args)) }): $t } =>
          println(e.show)
          println(f.showExtractors)
          args.foreach(a => println(a.show))
          println(t.show)

          // val argss = args.map(arg => lift(arg.seal, false)(env))
          // val call = Apply(f, argss).seal.cast[T]

          // '{ () => ${call} }.cast[Exp[Unit => T]]

          val ret = '{ () => println(${lift(args.head.seal, false)(env)}) }
          // println(ret.show)
          ret.cast[Exp[T]]

        case Const(value: Int) => '{
          $sym.Constant[Int](${value.toExpr})
        }.cast[Exp[T]]

        case Sym(b) if env.contains(b) =>
          // TODO: symbol table lookup
          // println("Symbol table")
          // env.foreach(v => println(v._1.name + " --> " + v._2.show))
          // println(e.show)
          val ret = '{ () => ${ env(b).cast[Var[T]] }.value }
          // println("# " + ret.show)
          ret.cast[Exp[T]]

        case _ =>
          println(e.show)

          summon[QuoteContext].error("Lifting error: " + e.show, e)
          '{ ??? }.cast[Exp[T]]
      }
    }

    val ret = '{
      ($sym).Method(${lift(a, true)(Map.empty)})
    }

    println(ret.show)
    println
    println

    ret
  }
}

case class Var[T](var value: T){
  def update(newVal: T) = value = newVal
}

trait Symantics[Ret, Exp[_]] {
  def Method(body: Exp[Ret]): Ret

  def Return(exp: Exp[Ret]): Exp[Ret]

  def ValDef[T, R](exp: Exp[T], body: Var[T] => Exp[R]): Exp[R]

  def If[T](exp: Exp[Boolean], thenp: Exp[T], elsep: Exp[T]): Exp[T]

  // def App[A, B](f: Exp[A => B], arg: Exp[A]): Exp[B]

  def While(exp: Exp[Boolean], body: Exp[Unit]): Exp[Unit]

  def Block[T1, T2] (one: Exp[T1], two: Exp[T2]): Exp[T2]

  def Constant[T](value: T): Exp[T]

  def Gt(lhs: Exp[Int], rhs: Exp[Int]): Exp[Boolean]

  def Lt(lhs: Exp[Int], rhs: Exp[Int]): Exp[Boolean]

  def Plus(arg1: Exp[Int], arg2: Exp[Int]): Exp[Int]

  def Mul(arg1: Exp[Int], arg2: Exp[Int]): Exp[Int]

  def Assign[T](lhs:  Var[T], rhs: Exp[T]): Exp[Unit]
}

