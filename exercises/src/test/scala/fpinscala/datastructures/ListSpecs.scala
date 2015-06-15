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
}