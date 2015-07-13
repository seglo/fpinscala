package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  // answer 3.7:
  // a) foldRight will continue to recurse even when encountering a 0 because foldRight
  //    has no special case for 0
  // b) foldRight is not tail recursive so it will add as many recursive calls to the
  //    call stack as there are elements in the list
  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else drop(tail(l), n-1)

//  attempt #1
//  NOTE: I misunderstood the assignment.  Only asks to drop elements from the
//  start of the array until it hits an element that doesn't satisfy predicate.
//  My solution drops all elements in an array that match predicate.
//  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
//    def go(acc: List[A], items: List[A]): List[A] = items match {
//      case Nil => acc
//      case Cons(x, xs) =>
//        if (f(x))
//          go(acc, xs)
//        else
//          go(append(acc, List(x)), xs)
//    }
//    go(List[A](), l)
//  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    def go(x: List[A], y: List[A]): List[A] = {
      x match {
        case Nil => Nil
        case Cons(x, Nil) => y
        case Cons(x, xs) => go(xs, List.append(y, List(x)))
      }
    }
    go(l, Nil)
  }

  // can't infer type because acc is 2nd parameter?
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((e: A, acc: Int) => acc + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def foldSum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def foldProduct(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  def foldLength[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil:List[A])((acc, e) => Cons(e, acc))

  def foldAppend[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)((e, acc) => Cons(e, acc))

  def flatten[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])((e, acc) => foldAppend(e, acc))

  def addOne(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((e, acc) => Cons(e+1, acc))

  def castToString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((e, acc) => Cons(e.toString, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((e, acc) => Cons(f(e), acc))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil:List[A]) { // predicate function scope
      (e, acc) =>
        if (f(e)) Cons(e, acc) // matches predicate
        else acc
    }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    // map over initial list
    foldRight(as, Nil:List[B]) {
      // apply function over each item in list and then map over its values
      (e, acc) => foldRight(f(e), acc) {
        (e2, acc2) => Cons(e2, acc2)
      }
    }

  def flatMapFilter[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l) { a =>
      if (f(a)) List(a)
      else Nil
    }

//  def zipAdd[Int](l1: List[Int], l2: List[Int]) = {
//    l1 match {
//      case Nil =>
//      case Cons(h1:Int, t1:List[Int]) => l2 match {
//        case Cons(h2:Int, t2:List[Int]) => Cons(h1+h2, zipAdd(t1, t2))
//      }
//    }
//  }
}
