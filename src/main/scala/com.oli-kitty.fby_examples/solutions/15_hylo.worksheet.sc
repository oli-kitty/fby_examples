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

/**
  * Let's take a step back and look at our example
  */
val productOpA: Algebra[ListF[Int, ?], Int] = {
  case None         => 1
  case Some((x, y)) => x * y
}

val rangeOpC: Coalgebra[ListF[Int, ?], Int] =
  n => if (n <= 0) None else Some((n, n - 1))

val rangeList: Int => List[Int] = ana(rangeOpC)(embedListA)
val productList: List[Int] => Int = cata(productOpA)(projectListC)

// we can compose cata and ana!
def factorial: Int => Int = productList compose rangeList
factorial(4)

/*
 *
 * This operation, unfolding a data structure from a seed value (coalgebra),
 * then computing a final result by folding over the data structure (algebra),
 * is called a hylomorphism (or shorter, hylo).
 */
// implement hylo
def hylo[F[_]: Functor, A, B](
    algebra: Algebra[F, B],
    coalgebra: Coalgebra[F, A]
): A => B =
  new (A => B) { kernel =>
    def apply(init: A): B =
      algebra(coalgebra(init).fmap(kernel))
  }

// Implement factorial using hylo
hylo(productOpA, rangeOpC).apply(4)

// Implement QuickSort using hylo

type QuickF[A, B] = Option[(A, (B, B))]

val conqueror: Algebra[QuickF[Int, ?], List[Int]] = {
  case None => Nil
  case Some((x, y)) => y._1 ::: x :: y._2
}

val divide: Coalgebra[QuickF[Int, ?], List[Int]] =
  _ match {
    case Nil          => None
    case head :: next => Some((head, next.partition(_ < head)))
  }

implicit def functor2[A]: Functor[λ[a => QuickF[A, a]]] =
  new Functor[λ[a => QuickF[A, a]]] {
    def map[B, C](fa: QuickF[A, B])(f: B => C) = fa.map(_.map(_.bimap(f, f)))
  }

hylo(conqueror, divide).apply(10 :: 6 :: 7 :: 2 :: Nil)
