/**
  * Implement unfold function
  */

def range(x: Int): Option[(Int, Int)] = if (x > 0) Some((x, x - 1)) else None

def unfold[E, B](init: B)(f: (B) => Option[(E, B)]): List[E] = ???

// using recursion
def unfold1[E, B](init: B)(f: (B) => Option[(E, B)]): List[E] = {
  f(init) match {
    case None          => Nil
    case Some((x, xs)) => x :: unfold1(xs)(f)
  }
}

unfold1(10)(range)

// improve recursion using a nested function
def unfold2[E, B](init: B)(f: (B) => Option[(E, B)]): List[E] = {
  def kernel(i: B): List[E] = {
    f(i) match {
      case None          => Nil
      case Some((xs, x)) => xs :: kernel(x)
    }
  }
  
  kernel(init)
}

unfold2(10)(range)

def unfold3[E, B](f: (B) => Option[(E, B)]): B => List[E] = {
  new (B => List[E]) { kernel => 
    def apply(i: B): List[E] = {
      f(i) match {
        case None          => Nil
        case Some((xs, x)) => xs :: kernel(x)
      }

    }
  }
}

unfold3(range)(10)
