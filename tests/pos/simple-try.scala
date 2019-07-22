object Test {
  def main(args: Array[String]) =
    try assert(args.nonEmpty)
    catch case ex: AssertionError => ()
}