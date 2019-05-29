package scala.tasty
package reflect.utils

import scala.quoted._

trait TreeUtils {

  val reflect: Reflection
  import reflect._

  /** Bind the `rhs` to a `val` and use it in `body` */
  def let(rhs: Term)(body: Ident => Term): Term = {
    val rhsSealedExpr: SealedExpr = rhs.seal2
    import implied rhsSealedExpr.tpe
//    implied for quoted.Type[rhsSealedExpr.T] = rhsSealedExpr.T
    val rhsExpr = rhsSealedExpr.expr
    val expr = '{
      val x = $rhsExpr
      ${
        val id = ('x).unseal.asInstanceOf[Ident]
        body(id).seal
      }
    }
    expr.unseal
  }

}
