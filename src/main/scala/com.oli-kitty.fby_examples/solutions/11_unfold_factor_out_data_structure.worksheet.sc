import cats._
import cats.implicits._

/**
  * Let's factor all steps out of the function
  */
type ListF[A, B] = Option[(A, B)]
implicit def functor[A]: Functor[ListF[A, ?]] = Functor[Option].compose[(A, ?)]

def embedList[E]: ListF[E, List[E]] => List[E] = {
  _ match {
    case None => Nil
    case Some((e, le)) => e :: le
  }
}

def range: Int => ListF[Int, Int] =
  v => if (v <= 0) None else Some((v, v - 1))

// reimplement unfold
def unfold[F[_]: Functor, S, A](f: (A) => F[A])(embed: F[S] => S): A => S =
  new (A => S) { kernel =>
    def apply(init: A): S = embed(f(init).fmap(kernel))
  }

unfold(range)(embedList).apply(10)