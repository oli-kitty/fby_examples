/**
  * It’s easy to see that foldRight takes more parameters than unfold. Can we do anything about it?
  * 
  * def foldRight6[A, E](op: Option[(E, A)] => A): List[E] => A = ???
  * def unfold[B, E](f: B => Option[(B, E)]): B => List[E] = ???
  */
val list = 5 :: 4 :: 1 :: Nil
val seed = 1
def product(x: Int, y: Int) = x * y

// make z a function
def foldRight1[A, E](z: () => A)(op: (E, A) => A): List[E] => A = {
  new (List[E] => A) { kernel =>
    def apply(v1: List[E]): A =
      v1 match {
        case Nil        => z()
        case head :: tl => op(head, kernel(tl))
      }
  }
}

foldRight1(() => seed)(product)(list)

// use Either, explain why it's equivalent
def foldRight2[A, E](op: Either[Unit, (E, A)] => A): List[E] => A = {
  new (List[E] => A) { kernel =>
    def apply(v1: List[E]): A =
      v1 match {
        case Nil        => op(Left(()))
        case head :: tl => op(Right((head, kernel(tl))))
      }
  }
}

// use Option, explain why it's equivalent
def foldRight3[A, E](op: Option[(E, A)] => A): List[E] => A = {
  new (List[E] => A) { kernel =>
    def apply(v1: List[E]): A =
      v1 match {
        case Nil        => op(None)
        case head :: tl => op(Some((head, kernel(tl))))
      }
  }
}

def product(i: Option[(Int, Int)]): Int = i match {
  case None         => seed
  case Some((e, a)) => a * e
}

foldRight3(product)(list)
