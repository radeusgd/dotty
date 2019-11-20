
import scala.quoted._
import scala.quoted.matching._
import collection.mutable
import scala.tasty.Reflection
import scala.quoted.matching._

object Macros {

  inline def virtualize[Ret, Exp[_]](a: => Ret)(given sym: Symantics[Ret, Exp]) : Ret = ${  virtualizeImpl[Ret, Exp]('{a}, '{sym}) }

  class Key[+V]

  private val Frontier = new Key[Boolean]

  private def virtualizeImpl[Ret: Type, Exp[_]: Type](a: Expr[Ret], sym: Expr[Symantics[Ret, Exp]])(given qctx: QuoteContext): Expr[Ret] = {
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

    def lift[T: quoted.Type](e: Expr[T], last: Boolean)(ctx: Context): Expr[Exp[T]] = {
      println("transforming --> " + e.show)
      val transformed = e match {
        case '{ var $y_bind: $t = $z; $k:T } => '{
            $sym.ValDef[$t, T](${lift(z, false)(ctx)}, (y: Var[$t]) => ${lift(k, last)(ctx.update(y_bind, 'y))})
          }.cast[Exp[T]]

        case '{ (${ Unseal(Assign(lhs, rhs)) }): $t } =>
          val ret = ctx.get(new Sym(lhs.symbol.name, lhs.symbol)).get

          type TT
          implicit val ttype: quoted.Type[TT] = rhs.tpe.widen.seal.asInstanceOf[quoted.Type[TT]]

          '{
            $sym.Assign[TT](
              ${ret.cast[Var[TT]]},
              ${lift(rhs.seal.cast[TT], false)(ctx)})
          }.cast[Exp[T]]

        case '{ (if ($cond) $thenp else $elsep): $t } => '{
          $sym.If[$t](${lift(cond, false)(ctx)}, ${lift(thenp, false)(ctx)}, ${lift(elsep, false)(ctx)})
        }.cast[Exp[T]]

        case '{ (while($cond) $body) } => '{
            $sym.While(${lift(cond, false)(ctx)}, ${lift(body, false)(ctx)})
          }.cast[Exp[T]]

        case '{ ($stmt1: $t); $stmt2 } =>
          type TT = $t
          '{
            $sym.Block(${lift[TT](stmt1, true)(ctx)}, ${lift(stmt2, true)(ctx)})
          }.cast[Exp[T]]

        // case e if last => '{
        //     $sym.Return(  ${lift(e, false)(ctx).cast[Exp[T]]}.asInstanceOf[Exp[Ret]]  )
        //   }.cast[Exp[T]]

        case '{( $x: Int) + ($y: Int) } => '{
            $sym.Plus(${lift(x, false)(ctx)}, ${lift(y, false)(ctx)})
          }.cast[Exp[T]]

        case '{( $x: Int) > ($y: Int) } => '{
            $sym.Gt(${lift(x, false)(ctx)}, ${lift(y, false)(ctx)})
          }.cast[Exp[T]]

        case '{( $x: Int) < ($y: Int) } => '{
            $sym.Lt(${lift(x, false)(ctx)}, ${lift(y, false)(ctx)})
          }.cast[Exp[T]]

        case '{( $x: Int) * ($y: Int) } => '{
            $sym.Mul(${lift(x, false)(ctx)}, ${lift(y, false)(ctx)})
          }.cast[Exp[T]]

        case '{ (${ Unseal(Apply(f, arg :: Nil)) }): $t } =>
          type S

          implicit val sEv: quoted.Type[S] = arg.tpe.widen.seal.asInstanceOf[quoted.Type[S]]

          // f.tpe.widen match {
          //   case MethodType(_, paramType :: Nil, _) => paramType.seal.asInstanceOf[quoted.Type[S]]
          // }

          val liftedFunc = f.etaExpand.seal.cast[S => T]

          '{
            $sym.App($sym.Inject($liftedFunc), ${lift(arg.seal.cast[S], false)(ctx)})
          }

        case Const(value: Int) => '{
            $sym.Inject[Int](${value.toExpr})
          }.cast[Exp[T]]

        case Sym(b) if ctx.get(b).isDefined =>
          val varAccess = ctx.get(b).get.unseal.tpe.widen

          '{
            $sym.DeRef[T](${ctx.get(b).get}.asInstanceOf[Var[T]])
          }.cast[Exp[T]]

        case _ =>
          println(e.show)

          summon[QuoteContext].error("Lifting error: " + e.show, e)
          '{ ??? }.cast[Exp[T]]
      }

      println("transformed --> " + transformed.show)
      println

      transformed
    }

    val ret = '{
      ($sym).Method(${lift(a, true)(Context.create)})
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

  def App[A, B](f: Exp[A => B], arg: Exp[A]): Exp[B]

  def While(exp: Exp[Boolean], body: Exp[Unit]): Exp[Unit]

  def Block[T1, T2] (one: Exp[T1], two: Exp[T2]): Exp[T2]

  def Inject[T](value: T): Exp[T]

  def Gt(lhs: Exp[Int], rhs: Exp[Int]): Exp[Boolean]

  def Lt(lhs: Exp[Int], rhs: Exp[Int]): Exp[Boolean]

  def Plus(arg1: Exp[Int], arg2: Exp[Int]): Exp[Int]

  def Mul(arg1: Exp[Int], arg2: Exp[Int]): Exp[Int]

  def Assign[T](lhs:  Var[T], rhs: Exp[T]): Exp[Unit]

  def DeRef[T](x: Var[T]): Exp[T]
}

