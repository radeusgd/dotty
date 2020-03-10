object Test {
  import Naperians.{given _, _}

  def main(args: Array[String]): Unit = {
    val test: Pair[Int] = (1, 2)

    val res = naperianTuple.tabulate(pos => test.lookup(pos) + 41)

    println(res)
  }
}
