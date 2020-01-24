/**
  * So far, our implementation is tightly coupled to a list—our data structure.
  * While we cleaned up the code a bit, we didn’t generalize our functions.
  * To better understand where the List type appears, let’s break down our function into steps:  *
  * Unpacking/projecting data structure
  * Recursion: call self for the nested structure
  * Computation
  */

def foldRight[A, E](op: Option[(E, A)] => A): List[E] => A = {
  new (List[E] => A) { kernel =>
    def apply(v1: List[E]): A =
      v1 match {
        case Nil        => op(None)
        case head :: tl => op(Some((head, kernel(tl))))
      }
  }
}

// step 1: pattern match on list ~ unpack/project the data structure
def foldRight1[A, E](op: Option[(E, A)] => A): List[E] => A = {
  new (List[E] => A) { kernel =>
    def apply(v1: List[E]): A =
      v1 match {
        case Nil        => op(None)
        case head :: tl => op(Some((head, kernel(tl))))
      }
  }
}

// step 2: recursion
def foldRight2[A, E](op: Option[(E, A)] => A): List[E] => A = {
  new (List[E] => A) { kernel =>
    def apply(v1: List[E]): A =
      v1 match {
        case Nil        => op(None)
        case head :: tl => op(Some((head, kernel(tl))))
      }
  }
}

// step 3: computation
def foldRight3[A, E](op: Option[(E, A)] => A): List[E] => A = {
  new (List[E] => A) { kernel =>
    def apply(v1: List[E]): A =
      v1 match {
        case Nil        => op(None)
        case head :: tl => op(Some((head, kernel(tl))))
      }
  }
}
