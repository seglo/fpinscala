package fpinscala.gettingstarted

import org.specs2.mutable.Specification
import MyModule._
import PolymorphicFunctions._

class GettingStartedSpecs extends Specification {
  "2.1) Fibonacci sequence" >> {
    "is 0 at index 0" >> {
      fib(0) mustEqual 0
    }
    "is 1 at index 1" >> {
      fib(1) mustEqual 1
    }
    "is 8 at index 6" >> {
      fib(6) mustEqual 8
    }
  }
  "2.2) isSorted Polymorphic function" >> {
    val sortedIntArray = Array(1, 2, 3, 4)
    val unsortedIntArray = Array(1, 2, 4, 3)
    val asc = (lh: Int, rh: Int) => lh < rh

    "should return true for empty array" >> {
      isSorted(Array[Int](), asc) must beTrue
    }
    "should return true for array with 1 element" >> {
      isSorted(Array(1), asc) must beTrue
    }
    "should detect sortedIntArray is sorted correctly" >> {
      isSorted(sortedIntArray, asc) must beTrue
    }
    "should detect unsortedIntArray is not sorted correctly" >> {
      isSorted(unsortedIntArray, asc) must beFalse
    }
  }

  "Other polymorphic HOF's" >> {
    "2.3) curry, add" >> {
      curry((a: Int, b: Int) => a + b)(1)(1) mustEqual 2
    }

    "2.4) uncurry, add" >> {
      uncurry((a: Int) => (b: Int) => a + b)(1, 1) mustEqual 2
    }

    "2.5) compose, pass through a value" >> {
      compose((a: Int) => a, (b: Int) => b)(0) mustEqual 0
    }
  }
}
