import cats._
import cats.implicits._

val list = 5 :: 4 :: 1 :: Nil
val seed = 1

def product(i: Option[(Int, Int)]): Int = i match {
  case None         => seed
  case Some((e, a)) => a * e
}

/**
  * Our goal is to get you accustomed to working with more abstract structures,
  * and develop the ability to recognize them. In the next code snippet,
  *  we’d like to introduce type aliases F[P] and S to decrease a distraction and focus on an abstraction.
  */
def foldRight[A, E](op: Option[(E, A)] => A): List[E] => A = {
  type F[P] = Option[(E, P)]
  type S = List[E]

  new (List[E] => A) { kernel =>
    def step1: List[E] => Option[(E, List[E])] = {
      _ match {
        case Nil          => None
        case head :: next => Some((head, next))
      }
    }
    def step2: Option[(E, List[E])] => Option[(E, A)] = {
      _ match {
        case None          => None
        case Some((x, xs)) => Some((x, kernel(xs)))
      }
    }
    def step3: Option[(E, A)] => A = op

    def apply(v1: List[E]): A =
      step3(step2(step1(v1)))
  }
}

foldRight(product)(list)

// use type aliases
def foldRight1[A, E](op: Option[(E, A)] => A): List[E] => A = {
  type F[P] = Option[(E, P)]
  type S = List[E]

  new (S => A) { kernel =>
    def step1: S => F[S] = {
      _ match {
        case Nil          => None
        case head :: next => Some((head, next))
      }
    }
    def step2: F[S] => F[A] = {
      _ match {
        case None          => None
        case Some((x, xs)) => Some((x, kernel(xs)))
      }
    }
    def step3: F[A] => A = op

    def apply(v1: S): A =
      step3(step2(step1(v1)))
  }
}

foldRight1(product)(list)

// use Functor
def foldRight2[A, E](op: Option[(E, A)] => A): List[E] => A = {
  type F[P] = Option[(E, P)]
  type S = List[E]
  implicit val F = Functor[Option].compose[(E, ?)]

  new (S => A) { kernel =>
    def step1: S => F[S] = {
      _ match {
        case Nil          => None
        case head :: next => Some((head, next))
      }
    }
    def step2: F[S] => F[A] = _.fmap(kernel)

    def step3: F[A] => A = op

    def apply(v1: S): A =
      step3(step2(step1(v1)))
  }
}

foldRight2(product)(list)
