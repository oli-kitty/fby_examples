/**
  * Reimplement foldRight.
  * Instead of having one function inside `apply`, let's introduce three new functions: one per each step.
  *
  * def foldRight0[A, E](op: Option[(E, A)] => A): List[E] => A = {
  *
  *   new (List[E] => A) { kernel =>
  *
  *     def step1: ??? => ??? = ???
  *     def step2: ??? => ??? = ???
  *     def step3: ??? => ??? = ???

  *     def apply(v1: List[E]): A =
  *       v1 match {
  *         case Nil        => op(None)
  *         case head :: tl => op(Some((head, kernel(tl))))
  *       }
  *   }
  * }
  */
val list = 5 :: 4 :: 1 :: Nil
val seed = 1

def product(i: Option[(Int, Int)]): Int = i match {
  case None         => seed
  case Some((e, a)) => a * e
}

// Provide signatures
def foldRight[A, E](op: Option[(E, A)] => A): List[E] => A = {
  new (List[E] => A) { kernel =>
    def step1: ??? => ??? = ???
    def step2: ??? => ??? = ???
    def step3: ??? => ??? = ???

    def apply(v1: List[E]): A =
      v1 match {
        case Nil        => op(None)
        case head :: tl => op(Some((head, kernel(tl))))
      }
  }
}

foldRight(product)(list)

// implement  step3
def foldRight2[A, E](op: Option[(E, A)] => A): List[E] => A = {
  new (List[E] => A) { kernel =>
    def step1: ??? => ??? = ???
    def step2: ??? => ??? = ???
    def step3: ??? => ??? = ???

    def apply(v1: List[E]): A =
      v1 match {
        case Nil        => op(None)
        case head :: tl => op(Some((head, kernel(tl))))
      }
  }
}

foldRight(product)(list)
foldRight2(product)(list)

// implement step1 & step2
def foldRight3[A, E](op: Option[(E, A)] => A): List[E] => A = {
  new (List[E] => A) { kernel =>
    def step1: ??? => ??? = ???
    def step2: ??? => ??? = ???
    def step3: ??? => ??? = ???

    def apply(v1: List[E]): A =
      v1 match {
        case Nil        => op(None)
        case head :: tl => op(Some((head, kernel(tl))))
      }
  }
}

foldRight(product)(list)
foldRight2(product)(list)
foldRight3(product)(list)
