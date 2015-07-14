package fpinscala.datastructures

import org.specs2.mutable.Specification

class ListSpecs extends Specification {
  "3.1) Result of match expression" >> {
    "will match case statement 3 and return 3" >> {
      List.x mustEqual 3
    }
  }
  "3.2) List tail" >> {
    "will return all elements after first" >> {
      val a = List(1,2,3)
      List.tail(a) mustEqual List(2,3)
    }
    "will return Nil for list with one element" >> {
      val a = List(1)
      List.tail(a) mustEqual Nil
    }
    // NOTE: a List with no empty arguments should return Nil. should Nil have a tail? should it throw an exception?
    "will return Nil for list with no elements" >> {
      val a = List()
      List.tail(a) mustEqual Nil
    }
  }
  "3.3) List setHead" >> {
    "will replace list with new head element" >> {
      val a = List(1,2,3)
      List.setHead(a, 0) mustEqual List(0,2,3)
    }
    "will return a list with head element if input list is empty" >> {
      val a = List()
      List.setHead(a, 0) mustEqual List(0)
    }
  }
  "3.4) List tail" >> {
    "will drop first element" >> {
      val a = List(1,2,3)
      List.drop(a, 1) mustEqual List(2,3)
    }
    "will return empty list when dropping size of list" >> {
      val a = List(1,2,3)
      List.drop(a, 3) mustEqual Nil
    }
    "will return empty list when dropping larger than size of list" >> {
      val a = List(1,2,3)
      List.drop(a, 4) mustEqual Nil
    }
  }
  "3.5) List dropWhile" >> {
    "will drop first two elements that matches predicate" >> {
      val a = List(1,2,3)
      val p = (v: Int) => v == 1 || v == 2
      List.dropWhile(a, p) mustEqual List(3)
    }
  }
  "3.6) List init" >> {
    "will return Nil when passed Nil" >> {
      List.init(Nil) mustEqual Nil
    }
    "will return Nil when it has a size of 1" >> {
      List.init(List(1)) mustEqual Nil
    }
    "will return all except the last element in a list" >> {
      val a = List(1,2,3)
      List.init(a) mustEqual List(1,2)
    }
  }
  // 3.7) Answers included in comments of product2.  Can't think of a
  // a test to include?  Maybe something that throws a stackoverflow
  // exception?
  "3.8) List foldRight with Nil accumulator" >> {
    "calling foldRight with Nil acc will return a copy of the list" >> {
      List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) mustEqual List(1,2,3)
    }
  }
  "3.9) List length" >> {
    "will return length of a list" >> {
      List.length(List(1,2,3)) mustEqual 3
    }
    "will return length of 0 given an empty list" >> {
      List.length(Nil) mustEqual 0
    }
  }
  "3.10) foldLeft" >> {
    "will return sum of List" >> {
      List.foldLeft(List(1,2,3), 0)(_ + _) mustEqual 6
    }
  }
  "3.11) foldLeft implementations of sum, product, and length" >> {
    "will return sum of List" >> {
      List.foldSum(List(1,2,3)) mustEqual 6
    }
    "will return product of List" >> {
      List.foldProduct(List(1,2,3)) mustEqual 6
    }
    "will return length of List" >> {
      List.foldLength(List(1,2,3)) mustEqual 3
    }
  }
  "3.12) foldLeft implementation of reverse" >> {
    "will return reversed elements in a list" >> {
      List.reverse(List(1,2,3)) mustEqual List(3,2,1)
    }
  }
  "3.14) foldRight implementation of append" >> {
    "will append two lists together" >> {
      List.foldAppend(List(1,2,3), List(4,5,6)) mustEqual List(1,2,3,4,5,6)
    }
  }
  "3.15) flatten/append" >> {
    "will flatten a list of lists" >> {
      List.flatten(List(List(1,2),List(3,4),List(5,6))) mustEqual List(1,2,3,4,5,6)
    }
  }
  "3.16) addOne" >> {
    "will add 1 to every element of a list" >> {
      List.addOne(List(1,2,3)) mustEqual List(2,3,4)
    }
  }
  "3.17) castToString" >> {
    "will cast a List[Double] to List[String]" >> {
      List.castToString(List(1.1, 2.2)) mustEqual List("1.1", "2.2")
    }
  }
  "3.18) map" >> {
    "will return a square of each element" >> {
      List.map(List(1,2,3))(a => a*a) mustEqual List(1, 4, 9)
    }
  }
  "3.19) filter" >> {
    "will return only even numbers" >> {
      List.filter(List(1,2,3))(_%2==0) mustEqual List(2)
    }
  }
  "3.20) flatMap" >> {
    "will return a list of duplicated values" >> {
      List.flatMap(List(1,2,3))(i => List(i,i)) mustEqual List(1,1,2,2,3,3)
    }
  }

  "3.21) flatMapFilter" >> {
    "will return only even numbers" >> {
      List.filter(List(1,2,3))(_%2==0) mustEqual List(2)
    }
  }

  "3.22) zipAdd" >> {
    "will add each element of each list in position" >> {
      List.zipAdd(List(1,2,3), List(4,5,6)) mustEqual List(5,7,9)
    }
  }

  "3.23) zip" >> {
    "will zip using product operation" >> {
      List.zip(List(1,2,3), List(4,5,6))((x, y) => x*y) mustEqual List(4,10,18)
    }
  }

  "3.24) hasSubsequence" >> {
    "will return true when a subsequence is present" >> {
      List.hasSubsequence(List(1,2,3,4), List(2,3)) must beTrue
    }
    "will return false when a subsequence is not present" >> {
      List.hasSubsequence(List(1,2,3,4), List(9,9)) must beFalse
    }
  }

}
