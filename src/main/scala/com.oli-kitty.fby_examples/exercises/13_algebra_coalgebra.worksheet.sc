import cats._
import cats.implicits._

type ListF[A, B] = Option[(A, B)]
implicit def functor[A]: Functor[ListF[A, ?]] = Functor[Option].compose[(A, ?)]

/*
 * F[A] => A is often called Algebra in the category theory/recursion schemes literature,
 * and its dual A => F[A] is called Coalgebra. Let’s introduce aliases for these types:
 */
type Algebra[F[_], A] = F[A] => A
type Coalgebra[F[_], A] = A => F[A]

// use Algebra and Coalgebra
val productOpA: ??? = {
  case None => 1
  case Some((x, y)) => x * y
}
val rangeOpC: ??? =
  n => if (n <= 0) None else Some((n, n - 1))


def cata[F[_]: Functor, S, B](algebra: ???)(project: ???): S => B =
  new (S => B) { kernel =>
    def apply(input: S): B =
      algebra(project(input).fmap(kernel))
  }

def ana[F[_]: Functor, S, A](coalgebra: ???)(embed: ???): A => S =
  new (A => S) { kernel =>    
    def apply(init: A): S =
      embed(coalgebra(init).fmap(kernel))
  }  

def projectListC[A]: ???  = {
  case Nil => None
  case head :: tail => Some((head, tail))
}

def embedListA[A]: ??? = {
  case None => Nil
  case Some((head, tail)) => head :: tail
}

cata(productOpA)(projectListC).apply(1 :: 10 :: 20 :: Nil)
ana(rangeOpC)(embedListA).apply(10) 