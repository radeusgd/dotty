import scala.compiletime._

object Test {

  type Shape = Array[Int]

  abstract class UTensor[E] {
    val sh: Shape
    val buffer: Array[E]

    def get(indices: Index*): E = ???
    def add(other: UTensor[E]): UTensor[E] = ???
  }

  object UTensor {
    def apply(init: Float) : UTensor[Float] = ???
  }

  given as Conversion[Float, UTensor[Float]] {
     def apply(datum: Float): UTensor[Float] = UTensor(datum)
  }

  type Index = Int

  def get[E](array: UTensor[E], indices: Array[Index]): E = ???

  // def build[E](shape: Shape, elemShape: Shae, f : Array[Index] => Tensor[elemShape, E]): Tensor[Add[shape, elemShape], E]
  def build[E](shape: Shape, elemShape: Shape, f: Array[Index] => UTensor[E]): UTensor[E] = ???
  /*
   tmp = allocate_new_array(add(shape, elemShape));

   for (outer_idxs in (0...) to shape) {
    bah = f outer_idxs;
    for (inner_idxs in (0...) to elemShape) {
      tmp[outer_idxs,inner_idx] = bah[inner_idxs]
    }
   }

   return tmp;
  */

  def ifold[S](folder: (S, Array[Index]) => S, state: S, sh: Shape): S = ???
  /*
    s_var = s;
    for (idxs in (0...) to sh) {
      s_var += folder (s_var, idxs)
    }
    return s_var;
  */

  def getShape(tensor: UTensor[_]): Shape = tensor.sh
  /*

  */

  def ex1 () = {
    val fu = xs: Array[Index] => UTensor(0.0f)

    get(build(Array(1, 2), Array(3, 4), fu), Array(0,0,0,0)) // =:= fu((Array(0,0)))
  }

  def vmul(x: UTensor[Float], w: UTensor[Float]) : UTensor[Float] = {
    var vshape = x.sh
    var mshape = w.sh

    var mrows = mshape(0)
    var mcols = mshape(1)
    var vrows = vshape(0)

    build(Array(mcols), Array(),
      cid => ifold((s, rid) => s.add(w.get(rid(0), cid(0))), UTensor(0.0f), Array(mrows)))
  }

  def pointwise(x : UTensor[Float]) : UTensor[Float] = {
    build(x.sh, Array(), ind => x.get(ind:_*) * 0.5f)
  }

  def simple_mlp(x : UTensor[Float], w : UTensor[Float], u : UTensor[Float]) = {
    var t1 = vmul(x, w)
    var z = pointwise(t1)
    var t2 = vmul(z, u)
    pointwise(t2)
  }

  def map(x : UTensor[Float], i : Int, outShape : Shape, f : UTensor[Float] => UTensor[Float]) : UTensor[Float] = {
    build(take i x.sh ++ outShape, idxs =>  {
      f(x.partial_get(take i idxs)).get(drop i idxs)
    }
  }


  // // N x N, k x k x c => N x N x c
  // def conv(in: UTensor[Float], filters: UTensor[Float]): UTensor[Float] = {
  //  val c = filters.sh(2)

  //   val neigborhoods = build(in.sh, filters.sh, in_idxs =>
  //            build(filters.sh, Array(),
  //               fw_idxs => (in_idxs + fw_idxs < in.sh)?in[in_idxs + fw_idxs] : 0.0f))

  //   map(neigborhoods, 2, Array(1, c),
  // }

}