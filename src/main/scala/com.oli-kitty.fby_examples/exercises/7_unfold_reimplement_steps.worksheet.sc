def range(x: Int): Option[(Int, Int)] = if (x > 0) Some((x, x - 1)) else None

/*
def unfold0[E, B](f: (B) => Option[(E, B)]): B => List[E] = {
  new (B => List[E]) { kernel =>

    def step1: ??? => ??? = ???
    def step2: ??? => ??? = ???
    def step3: ??? => ??? = ???

    def apply(i: B): List[E] =
      f(i) match {
        case None          => Nil
        case Some((xs, x)) => xs :: kernel(x)
      }
  }
}

unfold0(range)(10)
 */

// provide signatures
def unfold1[E, B](f: (B) => Option[(E, B)]): B => List[E] = {
  new (B => List[E]) { kernel =>

    def step1: ??? => ??? = ???
    def step2: ??? => ??? = ???
    def step3: ??? => ??? = ???

    def apply(i: B): List[E] =
      f(i) match {
        case None          => Nil
        case Some((xs, x)) => xs :: kernel(x)
      }
  }
}

unfold1(range)(10)

// apply step1
def unfold2[E, B](f: (B) => Option[(E, B)]): B => List[E] = {
  new (B => List[E]) { kernel =>

    def step1: ??? => ??? = ???
    def step2: ??? => ??? = ???
    def step3: ??? => ??? = ???

    def apply(i: B): List[E] =
      f(i) match {
        case None          => Nil
        case Some((xs, x)) => xs :: kernel(x)
      }
  }
}

unfold2(range)(10)

// apply the rest
def unfold3[E, B](f: (B) => Option[(E, B)]): B => List[E] = {
  new (B => List[E]) { kernel =>

    def step1: ??? => ??? = ???
    def step2: ??? => ??? = ???
    def step3: ??? => ??? = ???

    def apply(i: B): List[E] =
      f(i) match {
        case None          => Nil
        case Some((xs, x)) => xs :: kernel(x)
      }
  }
}

unfold3(range)(10)
