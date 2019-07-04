object Test {
  import Strymonas._

  def main(args: Array[String]): Unit = {
    val test1 = Stream.of('{Array(1, 2, 3)}).fold(0, (a, b) => a + b)

  //   def test1() = Stream
  //   .of('{Array(1, 2, 3)})
  //   .fold(0, (a, b) => a + b)

  // def test2() = Stream
  //   .of('{Array(1, 2, 3)})
  //   .map(a => a * 2)
  //   .fold(0, (a, b) => a + b)

  // def test3() = Stream
  //   .of('{Array(1, 2, 3)})
  //   .flatMap(d => Stream.of('{Array(1, 2, 3)}).map(dp => '{ $d * $dp }))
  //   .fold('{0}, (a, b) => '{ $a + $b })

  // def test4() = Stream
  //   .of('{Array(1, 2, 3)})
  //   .filter(d => '{ $d % 2 == 0 })
  //   .fold('{0}, (a, b) => '{ $a + $b })

  // def test5() = Stream
  //   .of('{Array(1, 2, 3)})
  //   .take('{2})
  //   .fold('{0}, (a, b) => '{ $a + $b })

  // def test6() = Stream
  //   .of('{Array(1, 1, 1)})
  //   .flatMap((d) => Stream.of('{Array(1, 2, 3)}).take('{2}))
  //   .take('{5})
  //   .fold('{0}, (a, b) => '{ $a + $b })

  // def test7() = Stream
  //   .of('{Array(1, 2, 3)})
  //   .zip((a => (b : Expr[Int]) => '{ $a + $b }), Stream.of('{Array(1, 2, 3)}))
  //   .fold('{0}, (a, b) => '{ $a + $b })

  // def test8() = Stream
  //   .of('{Array(1, 2, 3)})
  //   .zip((a => (b : Expr[Int]) => '{ $a + $b }), Stream.of('{Array(1, 2, 3)}).flatMap(d => Stream.of('{Array(1, 2, 3)}).map(dp => '{ $d + $dp })))
  //   .fold('{0}, (a, b) => '{ $a + $b })

  // def test9() = Stream
  //   .of('{Array(1, 2, 3)}).flatMap((d: Expr[Int]) => Stream.of('{Array(1, 2, 3)}).map((dp: Int) => '{ $d + $dp }))
  //   .zip(((a : Expr[Int]) => (b : Expr[Int]) => '{ $a + $b }), Stream.of('{Array(1, 2, 3)}) )
  //   .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ $a + $b }))

  // def test10() = Stream
  //   .of('{Array(1, 2, 3)}).flatMap(d => Stream.of('{Array(1, 2, 3)}).map(dp => '{ $d + $dp }))
  //   .zip((a => (b : Expr[Int]) => '{ $a + $b }), Stream.of('{Array(1, 2, 3)}).flatMap(d=> Stream.of('{Array(1, 2, 3)}).map(dp => '{ $d + $dp })) )
  //   .fold('{0}, (a, b) => '{ $a + $b })

  }
}