import scala.quoted.{_, given}
import scala.quoted.staging._

object Test {
  def main(args: Array[String]): Unit = {
    given Toolbox = Toolbox.make(getClass.getClassLoader)
    def show(expr: (given QuoteContext) => Expr[_]): String = withQuoteContext(expr.show)
    println(show(('{ () => x(0) }).apply()))
    println(show(('{ (x1: Int) => x1 }).apply('{x(1)})))
    println(show(('{ (x1: Int, x2: Int) => x1 + x2 }).apply('{x(1)}, '{x(2)})))
    println(show(('{ (x1: Int, x2: Int, x3: Int) => x1 + x2 + x3 }).apply('{x(1)}, '{x(2)}, '{x(3)})))
    println(show(('{ (x1: Int, x2: Int, x3: Int, x4: Int) => x1 + x2 + x3 + x4 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)})))
    println(show(('{ (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int) => x1 + x2 + x3 + x4 + x5 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)})))
    println(show(('{ (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int) => x1 + x2 + x3 + x4 + x5 + x6 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)})))
    println(show(('{ (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)})))
    println(show(('{ (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)})))
    println(show(('{ (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)}, '{x(9)})))
    println(show(('{ (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)}, '{x(9)}, '{x(10)})))
    println(show(('{ (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)}, '{x(9)}, '{x(10)}, '{x(11)})))
    println(show(('{ (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)}, '{x(9)}, '{x(10)}, '{x(11)}, '{x(12)})))
    println(show(('{ (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)}, '{x(9)}, '{x(10)}, '{x(11)}, '{x(12)}, '{x(13)})))
    println(show(('{ (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)}, '{x(9)}, '{x(10)}, '{x(11)}, '{x(12)}, '{x(13)}, '{x(14)})))
    println(show(('{ (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)}, '{x(9)}, '{x(10)}, '{x(11)}, '{x(12)}, '{x(13)}, '{x(14)}, '{x(15)})))
    println(show(('{ (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)}, '{x(9)}, '{x(10)}, '{x(11)}, '{x(12)}, '{x(13)}, '{x(14)}, '{x(15)}, '{x(16)})))
    println(show(('{ (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)}, '{x(9)}, '{x(10)}, '{x(11)}, '{x(12)}, '{x(13)}, '{x(14)}, '{x(15)}, '{x(16)}, '{x(17)})))
    println(show(('{ (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)}, '{x(9)}, '{x(10)}, '{x(11)}, '{x(12)}, '{x(13)}, '{x(14)}, '{x(15)}, '{x(16)}, '{x(17)}, '{x(18)})))
    println(show(('{ (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int, x19: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)}, '{x(9)}, '{x(10)}, '{x(11)}, '{x(12)}, '{x(13)}, '{x(14)}, '{x(15)}, '{x(16)}, '{x(17)}, '{x(18)}, '{x(19)})))
    println(show(('{ (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int, x19: Int, x20: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)}, '{x(9)}, '{x(10)}, '{x(11)}, '{x(12)}, '{x(13)}, '{x(14)}, '{x(15)}, '{x(16)}, '{x(17)}, '{x(18)}, '{x(19)}, '{x(20)})))
    println(show(('{ (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int, x19: Int, x20: Int, x21: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + x21 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)}, '{x(9)}, '{x(10)}, '{x(11)}, '{x(12)}, '{x(13)}, '{x(14)}, '{x(15)}, '{x(16)}, '{x(17)}, '{x(18)}, '{x(19)}, '{x(20)}, '{x(21)})))
    println(show(('{ (x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int, x19: Int, x20: Int, x21: Int, x22: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + x21 + x22 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)}, '{x(9)}, '{x(10)}, '{x(11)}, '{x(12)}, '{x(13)}, '{x(14)}, '{x(15)}, '{x(16)}, '{x(17)}, '{x(18)}, '{x(19)}, '{x(20)}, '{x(21)}, '{x(22)})))

    println(show(('{ (given x1: Int) => x1 }).apply('{x(1)})))
    println(show(('{ (given x1: Int, x2: Int) => x1 + x2 }).apply('{x(1)}, '{x(2)})))
    println(show(('{ (given x1: Int, x2: Int, x3: Int) => x1 + x2 + x3 }).apply('{x(1)}, '{x(2)}, '{x(3)})))
    println(show(('{ (given x1: Int, x2: Int, x3: Int, x4: Int) => x1 + x2 + x3 + x4 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)})))
    println(show(('{ (given x1: Int, x2: Int, x3: Int, x4: Int, x5: Int) => x1 + x2 + x3 + x4 + x5 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)})))
    println(show(('{ (given x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int) => x1 + x2 + x3 + x4 + x5 + x6 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)})))
    println(show(('{ (given x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)})))
    println(show(('{ (given x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)})))
    println(show(('{ (given x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)}, '{x(9)})))
    println(show(('{ (given x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)}, '{x(9)}, '{x(10)})))
    println(show(('{ (given x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)}, '{x(9)}, '{x(10)}, '{x(11)})))
    println(show(('{ (given x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)}, '{x(9)}, '{x(10)}, '{x(11)}, '{x(12)})))
    println(show(('{ (given x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)}, '{x(9)}, '{x(10)}, '{x(11)}, '{x(12)}, '{x(13)})))
    println(show(('{ (given x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)}, '{x(9)}, '{x(10)}, '{x(11)}, '{x(12)}, '{x(13)}, '{x(14)})))
    println(show(('{ (given x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)}, '{x(9)}, '{x(10)}, '{x(11)}, '{x(12)}, '{x(13)}, '{x(14)}, '{x(15)})))
    println(show(('{ (given x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)}, '{x(9)}, '{x(10)}, '{x(11)}, '{x(12)}, '{x(13)}, '{x(14)}, '{x(15)}, '{x(16)})))
    println(show(('{ (given x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)}, '{x(9)}, '{x(10)}, '{x(11)}, '{x(12)}, '{x(13)}, '{x(14)}, '{x(15)}, '{x(16)}, '{x(17)})))
    println(show(('{ (given x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)}, '{x(9)}, '{x(10)}, '{x(11)}, '{x(12)}, '{x(13)}, '{x(14)}, '{x(15)}, '{x(16)}, '{x(17)}, '{x(18)})))
    println(show(('{ (given x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int, x19: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)}, '{x(9)}, '{x(10)}, '{x(11)}, '{x(12)}, '{x(13)}, '{x(14)}, '{x(15)}, '{x(16)}, '{x(17)}, '{x(18)}, '{x(19)})))
    println(show(('{ (given x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int, x19: Int, x20: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)}, '{x(9)}, '{x(10)}, '{x(11)}, '{x(12)}, '{x(13)}, '{x(14)}, '{x(15)}, '{x(16)}, '{x(17)}, '{x(18)}, '{x(19)}, '{x(20)})))
    println(show(('{ (given x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int, x19: Int, x20: Int, x21: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + x21 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)}, '{x(9)}, '{x(10)}, '{x(11)}, '{x(12)}, '{x(13)}, '{x(14)}, '{x(15)}, '{x(16)}, '{x(17)}, '{x(18)}, '{x(19)}, '{x(20)}, '{x(21)})))
    println(show(('{ (given x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int, x19: Int, x20: Int, x21: Int, x22: Int) => x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + x21 + x22 }).apply('{x(1)}, '{x(2)}, '{x(3)}, '{x(4)}, '{x(5)}, '{x(6)}, '{x(7)}, '{x(8)}, '{x(9)}, '{x(10)}, '{x(11)}, '{x(12)}, '{x(13)}, '{x(14)}, '{x(15)}, '{x(16)}, '{x(17)}, '{x(18)}, '{x(19)}, '{x(20)}, '{x(21)}, '{x(22)})))
  }

  def x(i: Int): Int = i
}
