
/*
 * Implement a function product which  multiplies all elements of a list
*/

val a = 10 :: 5 :: 1 :: Nil

// using a foreach
def productList(a: List[Int]): Int = {
  var res = 1

  a.foreach(i => res = res * i)
  res
}

productList(a)

// using recursion
def productList2(a: List[Int]): Int =  {
    a match {
        case Nil => 1
        case head :: tl => head * productList2(tl)
    }
}

productList2(a)

// using a standard scala library function
a.foldRight(1)(_ * _)

/*
 * Implement a function range which gives you a range of numbers starting with the given number to 1
*/
val b = 10

// using while loop
def range(b: Int): List[Int] = {
    var res: List[Int] = Nil
    var i = 1
    while (i <= b) {
        res = i :: res
        i = i + 1
    }
    res
}

range(b)

// using recursion
def range2(b: Int): List[Int] = {
    if (b > 0) b :: range2(b - 1) else Nil
}

range2(b)

// using a standard scala library function
List.unfold(10)(a => if (a > 0) Some((a, a - 1)) else None)