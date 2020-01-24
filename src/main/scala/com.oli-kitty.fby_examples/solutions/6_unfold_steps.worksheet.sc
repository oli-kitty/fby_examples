/*
 * Break down unfold into steps
 */
def unfold1[E, B](f: (B) => Option[(E, B)]): B => List[E] = {
  new (B => List[E]) { kernel =>
    def apply(i: B): List[E] =
      f(i) match {
        case None          => Nil
        case Some((xs, x)) => xs :: kernel(x)
      }
  }
}

//  step 1: computation
def unfold2[E, B](f: (B) => Option[(E, B)]): B => List[E] = {
  new (B => List[E]) { kernel =>
    def apply(i: B): List[E] = {
      f(i)
      ???
    }
  }
}

// step 2: recursion
def unfold3[E, B](f: (B) => Option[(E, B)]): B => List[E] = {
  new (B => List[E]) { kernel =>
    def apply(i: B): List[E] =
      f(i) match {
        case ??? => ???
        case ??? => ??? kernel (???)
      }
  }
}

// step 3: building a list ~ embedding into a data structure
def unfold4[E, B](f: (B) => Option[(E, B)]): B => List[E] = {
  new (B => List[E]) { kernel =>
    def apply(i: B): List[E] =
      f(i) match {
        case ??? => Nil
        case ??? => ??? :: ???
      }
  }
}
