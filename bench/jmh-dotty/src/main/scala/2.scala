package foo

import scala.quoted.{Expr => E, _}

object Test {
  // Eq.derived[ISB]
  // Eq.derived[OptionInt]
  implicit val eil: Eq[IList] = Eq.derived[IList]
}
