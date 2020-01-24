/**
  * Implement unfold function
  */

def range(x: Int): Option[(Int, Int)] = if (x > 0) Some((x, x - 1)) else None

// using recursion
def unfold1[E, B](init: B)(f: (B) => Option[(E, B)]): List[E] = {
  ???
}

unfold1(10)(range)

// improve recursion using a nested function
def unfold2[E, B](init: B)(f: (B) => Option[(E, B)]): List[E] = {
  ???
}

unfold2(10)(range)

def unfold3[E, B](f: (B) => Option[(E, B)]): B => List[E] = {
  ???
}

unfold3(range)(10)
