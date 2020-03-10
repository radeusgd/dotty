
trait HList
trait HNil extends HList
case object HNil extends HNil
case class :: [+H, +T <: HList] (hd: H, tl: T) extends HList

object Test extends App {
  type Concat[Xs <: HList, Ys <: HList] <: HList = Xs match {
    case HNil => Ys
    case x1 :: xs1 => x1 :: Concat[xs1, Ys]
  }

  def concat[Xs <: HList, Ys <: HList](xs: Xs, ys: Ys): Concat[Xs, Ys] =
    null.asInstanceOf

  val xs0 = concat(HNil, HNil)
}
