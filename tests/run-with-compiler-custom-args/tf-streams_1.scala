import scala.quoted._

object Test {

  sealed trait Var[T] {
    def get given QuoteContext: Expr[T]
    def update(x: Expr[T]) given QuoteContext: Expr[Unit]
  }

  object Var {
    def apply[T: Type, U: Type](init: Expr[T])(body: Var[T] => Expr[U]) given QuoteContext: Expr[U] = '{
      var x = $init
      ${
        body(
          new Var[T] {
            def get given QuoteContext: Expr[T] = 'x
            def update(e: Expr[T]) given QuoteContext: Expr[Unit] = '{ x = $e }
          }
        )
      }
    }
  }

  trait Producer[A] { self =>
    type St
    val card: Cardinality

    def init(k: St => Expr[Unit]) given QuoteContext: Expr[Unit]
    def step(st: St, k: (A => Expr[Unit])) given QuoteContext: Expr[Unit]
    def hasNext(st: St) given QuoteContext: Expr[Boolean]
  }

  enum Cardinality {
    case AtMost1
    case Many
  }
  import Cardinality._

  trait StagedStream[A]
  case class Linear[A](producer: Producer[A]) extends StagedStream[A]
  case class Nested[A, B](producer: Producer[B], nestedf: B => StagedStream[A]) extends StagedStream[A]

  case class Stream[A: Type](stream: StagedStream[Expr[A]]) {

    def fold[W: Type](z: Expr[W], f: ((Expr[W], Expr[A]) => Expr[W])) given QuoteContext: Expr[W] = {
      Var(z) { s: Var[W] =>
        foldRaw[Expr[A]]((a: Expr[A]) => s.update(f(s.get, a)), stream)
        s.get
      }
    }

    private def foldRaw[A](consumer: A => Expr[Unit], stream: StagedStream[A]) given QuoteContext: Expr[Unit] = {
      stream match {
        case Linear(producer) => {
          producer.card match {
            case Many =>
              producer.init(sp => '{
                while(${producer.hasNext(sp)}) {
                  ${producer.step(sp, consumer)}
                }
              })
          }
        }
        case nested: Nested[A, bt] => {
          foldRaw[bt](((e: bt) => foldRaw[A](consumer, nested.nestedf(e))), Linear(nested.producer))
        }
      }
    }

    def map[B : Type](f: (Expr[A] => Expr[B])): Stream[B] = {
      Stream(mapRaw[Expr[A], Expr[B]](a => k => k(f(a)), stream))
    }

    private def mapRaw[A, B](f: (A => (B => Expr[Unit]) => Expr[Unit]), stream: StagedStream[A]): StagedStream[B] = {
      stream match {
        case Linear(producer) => {
          val prod = new Producer[B] {

            type St = producer.St
            val card = producer.card

            def init(k: St => Expr[Unit]) given QuoteContext: Expr[Unit] = {
              producer.init(k)
            }

            def step(st: St, k: (B => Expr[Unit])) given QuoteContext: Expr[Unit] = {
              producer.step(st, el => f(el)(k))
            }

            def hasNext(st: St) given QuoteContext: Expr[Boolean] = {
              producer.hasNext(st)
            }
          }

          Linear(prod)
        }
        case nested: Nested[A, bt] => {
          Nested(nested.producer, (a: bt) => mapRaw[A, B](f, nested.nestedf(a)))
        }
      }
    }
  }

  object Stream {
    def of[A: Type](arr: Expr[Array[A]]) given QuoteContext: Stream[A] = {
      val prod = new Producer[Expr[A]] {
        type St = (Var[Int], Var[Int], Expr[Array[A]])

        val card = Many

        def init(k: St => Expr[Unit]) given QuoteContext: Expr[Unit] = {
          Var('{($arr).length}) { n =>
            Var(0){ i =>
              k((i, n, arr))
            }
          }
        }

        def step(st: St, k: (Expr[A] => Expr[Unit])) given QuoteContext: Expr[Unit] = {
          val (i, _, arr) = st
          '{
              val el = ($arr).apply(${i.get})
              ${i.update('{ ${i.get} + 1 })}
              ${k('el)}
          }
        }

        def hasNext(st: St) given QuoteContext: Expr[Boolean] =  {
          val (i, n, _) = st
          '{
              (${i.get} < ${n.get})
          }
        }
      }

      Stream(Linear(prod))
    }
  }

  def test1() given QuoteContext = Stream
    .of('{Array(1, 2, 3)})
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ $a + $b }))

  def main(args: Array[String]): Unit = {
    given toolbox as scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

    println(run(test1()))
    println
  }
}
