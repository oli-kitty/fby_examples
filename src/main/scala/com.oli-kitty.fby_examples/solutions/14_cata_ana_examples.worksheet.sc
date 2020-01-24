import cats._
import cats.implicits._

type ListF[A, B] = Option[(A, B)]
implicit def functor[A]: Functor[ListF[A, ?]] = Functor[Option].compose[(A, ?)]

type Algebra[F[_], A] = F[A] => A
type Coalgebra[F[_], A] = A => F[A]

def cata[F[_]: Functor, S, B](
    algebra: Algebra[F, B]
)(project: Coalgebra[F, S]): S => B =
  new (S => B) { kernel =>
    def apply(input: S): B =
      algebra(project(input).fmap(kernel))
  }

def ana[F[_]: Functor, S, A](
    coalgebra: Coalgebra[F, A]
)(embed: Algebra[F, S]): A => S =
  new (A => S) { kernel =>
    def apply(init: A): S =
      embed(coalgebra(init).fmap(kernel))
  }

def projectListC[A]: Coalgebra[ListF[A, ?], List[A]] = {
  case Nil          => None
  case head :: tail => Some((head, tail))
}

def embedListA[A]: Algebra[ListF[A, ?], List[A]] = {
  case None               => Nil
  case Some((head, tail)) => head :: tail
}

/*
 * Throughout the project we used these two examples
 */
val productOpA: Algebra[ListF[Int, ?], Int] = {
  case None         => 1
  case Some((x, y)) => x * y
}

val rangeOpC: Coalgebra[ListF[Int, ?], Int] =
  n => if (n <= 0) None else Some((n, n - 1))

cata(productOpA)(projectListC).apply(1 :: 10 :: 20 :: Nil)
ana(rangeOpC)(embedListA).apply(10)

// Let's implement something else
val unsorted = 10 :: 80 :: 5 :: Nil

// Insertion sort using Cata
val insert: Algebra[ListF[Int, ?], List[Int]] = {
  case None         => Nil
  case Some((x, y)) => {
    val parts = y.partition(_ <= x)
    parts._1 ::: x :: parts._2
  }
}

cata(insert)(projectListC).apply(unsorted)

// Selection sort using Ana
def select: Coalgebra[ListF[Int, ?], List[Int]] = {
  case Nil => None
  case xs: List[Int] => {
    import scala.math.Ordering._
    val min = xs.min
    Some((min, xs.diff(List(min))))
  }
}

ana(select)(embedListA).apply(unsorted)
