import scala.quoted._

def foo(given QuoteContext) = {
  '{
    type X
    ${
      val t1: Type[X] = '[X]
      '{
        val x2: $t1 = ???
      }
    }
  }

  '{
    val x: Int = ???
    ${
      val t2 = '[x.type]
      '{
        val x2: $t2 = x
      }
    }
  }

  '{
    val f: Foo = ???
    ${
      val t3 = '[f.Y]
      '{
        val x2: $t3 = f.y
      }
    }
  }
}

trait Foo {
  type Y
  def y: Y
}

