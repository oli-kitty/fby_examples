import cats._
import cats.implicits._

type ListF[A, B] = Option[(A, B)]
implicit def functor[A]: Functor[ListF[A, ?]] = Functor[Option].compose[(A, ?)]

def foldRight[F[_]: Functor, S, B](f: F[B] => B)(project: S => F[S]): S => B = {
  new (S => B) { kernel =>
    def apply(init: S): B = f(project(init).fmap(kernel))
  }
}

def unfold[F[_]: Functor, S, A](f: (A) => F[A])(embed: F[S] => S): A => S =
  new (A => S) { kernel =>
    def apply(init: A): S = embed(f(init).fmap(kernel))
  }

def projectList[E]: List[E] => ListF[E, List[E]] = {
  _ match {
    case Nil          => None
    case head :: tail => Some((head, tail))
  }
}

/**
  * Whenever you generalize functions like this, take a critical look at your generalized function when you have finished.
  * Although the function may have been motivated by some specific use case(List), 
  * the signature and implementation may have a more general meaning.
  *  In this case, foldRight and unfold are perhaps no longer the most appropriate names for those functions.
  *  These functions, which often come up in recursion schemes libraries, could be called cata and ana.
 */

def cata[F[_]: Functor, S, B](f: F[B] => B)(project: S => F[S]): S => B =
  new (S => B) { kernel =>
    def apply(init: S): B = f(project(init).fmap(kernel))
  }

def ana[F[_]: Functor, S, A](f: (A) => F[A])(embed: F[S] => S): A => S =
  new (A => S) { kernel =>
    def apply(init: A): S = embed(f(init).fmap(kernel))
  }

def prodFlist: ListF[Int, Int] => Int = {
  _ match {
    case None         => 1
    case Some((x, y)) => x * y
  }
}

val list = 5 :: 4 :: 1 :: Nil

def embedList[E]: ListF[E, List[E]] => List[E] = {
  _ match {
    case None          => Nil
    case Some((e, le)) => e :: le
  }
}

def range: Int => ListF[Int, Int] =
  v => if (v <= 0) None else Some((v, v - 1))

foldRight(prodFlist)(projectList).apply(list)

unfold(range)(embedList).apply(10)

cata(prodFlist)(projectList).apply(list)

ana(range)(embedList).apply(10)

