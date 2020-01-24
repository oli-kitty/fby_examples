/**
  * Implement foldRight function
  */
val list = 5 :: 4 :: 1 :: Nil
val seed = 1
def product(x: Int, y: Int) = x * y

def foldRight[A, E](init: List[E])(z: A)(op: (E, A) => A): A = ???

// using a recursion
def foldRight1[A, E](init: List[E])(z: A)(op: (E, A) => A): A = {
  init match {
    case Nil        => z
    case head :: tl => op(head, foldRight1(tl)(z)(op))
  }
}

foldRight1(list)(seed)(product)

// improve recursion using a nested function
def foldRight2[A, E](init: List[E])(z: A)(op: (E, A) => A): A = {
  def kernel(i: List[E]): A = {
    i match {
      case Nil        => z
      case head :: tl => op(head, kernel(tl))
    }
  }
  kernel(init)
}

foldRight2(list)(seed)(product)

// Let's return a function
def foldRight3[A, E](z: A)(op: (E, A) => A): List[E] => A = {
  def kernel(i: List[E]): A = {
    i match {
      case Nil        => z
      case head :: tl => op(head, kernel(tl))
    }
  }
  kernel
}

foldRight3(seed)(product)(list)

// use anonymous function
def foldRight4[A, E](z: A)(op: (E, A) => A): List[E] => A = {
  new (List[E] => A) { kernel =>
    def apply(v1: List[E]): A =
      v1 match {
        case Nil        => z
        case head :: tl => op(head, kernel(tl))
      }
  }
}

foldRight4(seed)(product)(list)
