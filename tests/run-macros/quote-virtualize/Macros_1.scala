
import scala.quoted._
import scala.quoted.matching._
import collection.mutable
import scala.tasty.Reflection
import scala.quoted.matching._

object Macros {

  inline def virtualize[R, M[_]](a: => R)(given sym: Symantics[R, M]) : R = ${  virtualizeImpl[R, M]('{a}, '{sym}) }

  class Key[+V]

  private val Frontier = new Key[Boolean]

  private def virtualizeImpl[R: Type, M[_]: Type](a: Expr[R], sym: Expr[Symantics[R, M]])(given qctx: QuoteContext): Expr[R] = {
    import qctx.tasty.{_, given}

    type Env = mutable.Map[Sym[_], Expr[Var[_]]]

    class Context { thiscontext =>

      var outer: Context = null
      val env: Env = mutable.Map.empty
      var moreProperties: Map[Key[Any], Any] = Map.empty

      def init(outer: Context, origin: Context): this.type = {
        thiscontext.outer = outer
        thiscontext.moreProperties = origin.moreProperties
        this
      }

      def update(sym: Sym[_], varExpr: Expr[Var[_]]) = {
        env += (sym -> varExpr)
        this
      }

      def get(sym: Sym[_]): Option[Expr[Var[_]]] = {
        for (ctx <- outersIterator) {
          if(ctx.env.get(sym).isDefined)
            return ctx.env.get(sym)
        }
        None
      }

      def contains(sym:Sym[_]) = env.contains(sym)

      def outersIterator: Iterator[Context] = new Iterator[Context] {
        var current = thiscontext
        def hasNext = current != NoContext
        def next = { val c = current; current = current.outer; c }
      }

      def property[T](key: Key[T]): Option[T] = moreProperties.get(key).asInstanceOf[Option[T]]

      private def setMoreProperties(moreProperties: Map[Key[Any], Any]): this.type = { this.moreProperties = moreProperties; this }

      def setProperty[T](key: Key[T], value: T): this.type =
        setMoreProperties(moreProperties.updated(key, value))

      def dropProperty(key: Key[?]): this.type =
        setMoreProperties(moreProperties - key)

      final def withProperty[T](key: Key[T], value: Option[T]): Context =
        if (property(key) == value) this
        else value match {
          case Some(v) => fresh.setProperty(key, v)
          case None => fresh.dropProperty(key)
        }

      def fresh: Context = freshOver(this)

      def freshOver(outer: Context): Context = new Context().init(outer, this)
    }

    object Context {
      def create = {
        val ctx = new Context()
        ctx.outer = NoContext
        ctx
      }

    }

    object NoContext extends Context {}

    object Unseal {
      def unapply[T](e: Expr[T])(given qctx: QuoteContext) = {
        import qctx.tasty._
        Some(e.unseal)
      }
    }

    def rewrite[T: quoted.Type](e: Expr[T], last: Boolean)(ctx: Context): Expr[M[T]] = {
      println("transforming --> " + e.show)
      val transformed = e match {
        case '{ var $y_bind: $t = $z; $k:T } => '{
            $sym.Bind[$t, T](${rewrite(z, false)(ctx)}, (y: Var[$t]) => ${rewrite(k, last)(ctx.update(y_bind, 'y))})
          }.cast[M[T]]

        case '{ (${ Unseal(Assign(lhs, rhs)) }): $t } =>
          val ret = ctx.get(new Sym(lhs.symbol.name, lhs.symbol)).get

          type TT
          implicit val ttype: quoted.Type[TT] = rhs.tpe.widen.seal.asInstanceOf[quoted.Type[TT]]

          '{
            $sym.Assign[TT](
              ${ret.cast[Var[TT]]},
              ${rewrite(rhs.seal.cast[TT], false)(ctx)})
          }.cast[M[T]]

        case '{ (if ($cond) $thenp else $elsep): $t } => '{
          $sym.If[$t](${rewrite(cond, false)(ctx)}, ${rewrite(thenp, false)(ctx)}, ${rewrite(elsep, false)(ctx)})
          }.cast[M[T]]

        case '{ (while($cond) $body) } => '{
            $sym.While(${rewrite(cond, false)(ctx)}, ${rewrite(body, false)(ctx)})
          }.cast[M[T]]

        case '{ ($stmt1: $t); $stmt2 } =>
          type TT = $t
          '{
            $sym.Combine(${rewrite[TT](stmt1, true)(ctx)}, ${rewrite(stmt2, true)(ctx)})
          }.cast[M[T]]

        // case e if last => '{
        //     $sym.Return(  ${lift(e, false)(ctx).cast[Exp[T]]}.asInstanceOf[Exp[Ret]]  )
        //   }.cast[Exp[T]]

        case '{( $x: Int) + ($y: Int) } => '{
            $sym.Plus(${rewrite(x, false)(ctx)}, ${rewrite(y, false)(ctx)})
          }.cast[M[T]]

        case '{( $x: Int) > ($y: Int) } => '{
            $sym.Gt(${rewrite(x, false)(ctx)}, ${rewrite(y, false)(ctx)})
          }.cast[M[T]]

        case '{( $x: Int) < ($y: Int) } => '{
            $sym.Lt(${rewrite(x, false)(ctx)}, ${rewrite(y, false)(ctx)})
          }.cast[M[T]]

        case '{( $x: Int) * ($y: Int) } => '{
            $sym.Mul(${rewrite(x, false)(ctx)}, ${rewrite(y, false)(ctx)})
          }.cast[M[T]]

        case '{ (${ Unseal(Apply(f, arg :: Nil)) }): $t } =>
          type S

          implicit val sEv: quoted.Type[S] = arg.tpe.widen.seal.asInstanceOf[quoted.Type[S]]

          val liftedFunc = f.etaExpand.seal.cast[S => T]

          '{
            $sym.App($sym.Inject($liftedFunc), ${rewrite(arg.seal.cast[S], false)(ctx)})
          }

        case Const(value: Int) => '{
            $sym.Inject[Int](${value.toExpr})
          }.cast[M[T]]

        case Sym(b) if ctx.get(b).isDefined =>
          val varAccess = ctx.get(b).get.unseal.tpe.widen

          '{
            $sym.DeRef[T](${ctx.get(b).get}.asInstanceOf[Var[T]])
          }.cast[M[T]]

        case _ =>
          println(e.show)

          summon[QuoteContext].error("Lifting error: " + e.show, e)
          '{ ??? }.cast[M[T]]
      }

      println("transformed --> " + transformed.show)
      println

      transformed
    }

    val ret = '{
      ($sym).Method(${rewrite(a, true)(Context.create)})
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

trait Symantics[R, M[_]] {
  def Method(body: M[R]): R

  def Return(exp: M[R]): M[R]

  def Bind[T, R](exp: M[T], body: Var[T] => M[R]): M[R]

  def If[T](exp: M[Boolean], thenp: M[T], elsep: M[T]): M[T]

  def App[A, B](f: M[A => B], arg: M[A]): M[B]

  def While(exp: M[Boolean], body: M[Unit]): M[Unit]

  def Combine[T1, T2] (one: M[T1], two: M[T2]): M[T2]

  def Inject[T](value: T): M[T]

  def Gt(lhs: M[Int], rhs: M[Int]): M[Boolean]

  def Lt(lhs: M[Int], rhs: M[Int]): M[Boolean]

  def Plus(arg1: M[Int], arg2: M[Int]): M[Int]

  def Mul(arg1: M[Int], arg2: M[Int]): M[Int]

  def Assign[T](lhs:  Var[T], rhs: M[T]): M[Unit]

  def DeRef[T](x: Var[T]): M[T]
}

