import cats._
import cats.implicits._

def range(x: Int): Option[(Int, Int)] = if (x > 0) Some((x, x - 1)) else None
/**
  * Our goal is to get you accustomed to working with more abstract structures, 
  * and develop the ability to recognize them. In the next code snippet,
  *  we’d like to introduce type aliases F[P] and S to decrease a distraction and focus on an abstraction.
  */
def unfold0[E, B](f: (B) => Option[(E, B)]): B => List[E] = {
  type F[P] = Option[(E, P)]
  type S = List[E]

  new (B => List[E]) { kernel =>

    def step1: B => Option[(E, B)] = f
    def step2: Option[(E, B)] => Option[(E, List[E])] = _ match {
      case None          => None
      case Some((x, xs)) => Some((x, kernel(xs)))
    }
    def step3: Option[(E, List[E])] => List[E] = _ match {
      case None          => Nil
      case Some((x, xs)) => x :: xs
    }

    def apply(i: B): List[E] =
      step3(step2(step1(i)))
  }
}

unfold0(range)(10)

// use type aliases
def unfold1[E, B](f: (B) => Option[(E, B)]): B => List[E] = {
  type F[P] = Option[(E, P)]
  type S = List[E]

  new (B => S) { kernel =>

    def step1: B => F[B] = f
    def step2: F[B] => F[S] = _ match {
      case None          => None
      case Some((x, xs)) => Some((x, kernel(xs)))
    }
    def step3: F[S] => S = _ match {
      case None          => Nil
      case Some((x, xs)) => x :: xs
    }

    def apply(i: B): S =
      step3(step2(step1(i)))
  }
}

unfold1(range)(10)

// use Functor
def unfold2[E, B](f: (B) => Option[(E, B)]): B => List[E] = {
  type F[P] = Option[(E, P)]
  type S = List[E]

  implicit val F = Functor[Option].compose[(E, ?)]

  new (B => S) { kernel =>

    def step1: B => F[B] = f
    def step2: F[B] => F[S] = _.fmap(kernel)

    def step3: F[S] => S = _ match {
      case None          => Nil
      case Some((x, xs)) => x :: xs
    }

    def apply(i: B): S =
      step3(step2(step1(i)))
  }
}

unfold2(range)(10)