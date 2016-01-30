package fpinscala.errorhandling

import fpinscala.gettingstarted.{PolymorphicFunctions, MyModule}
import org.specs2.mutable.Specification
import fpinscala.gettingstarted.MyModule._

import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

class ErrorHandlingSpecs extends Specification {
  "4.1) Option type" >> {
    "map perform an operation over the value when it exists" >> {
      val o = Some(10)
      val newO = o.map { v => v * 10 }

      newO mustEqual Some(100)
    }

    "map perform an operation over a value that DNE" >> {
      // None implements Option as Option[Nothing], need treat as Option of Int to
      // perform map operation successfully
      val o: Option[Int] = None
      val newO = o.map { v => v * 10 }

      newO mustEqual None
    }

    "getOrElse returns the value when it exists" >> {
      val o = Some(10)
      o.getOrElse(0) mustEqual 10
    }

    "getOrElse returns the default value when it DNE" >> {
      val o: Option[Int] = None
      o.getOrElse(0) mustEqual 0
    }

    "flatMap performs an operation over a value that exists" >> {
      val o = Some(10)
      val newO = o.flatMap { v => Some(v * 10) }

      newO mustEqual Some(100)
    }

    // Q: when is an appropriate use case for Option.flatMap?
    "flatMap perform an operation over a value that DNE" >> {
      val o: Option[Int] = None
      val newO = o.flatMap { v => None }

      newO mustEqual None
    }

    "orElse returns another value when value is None" >> {
      val o: Option[Int] = None
      o.orElse(Some(0)) mustEqual Some(0)
    }

    "filter flips Option to None when value matches predicate" >> {
      val o: Option[Int] = Some(2)
      o.filter(v => v % 2 == 1) mustEqual None
    }

    "filter does nothing to Option when value doesn't match predicate" >> {
      val o: Option[Int] = Some(2)
      o.filter(v => v % 2 == 0) mustEqual Some(2)
    }

    "filter does nothing to Option when value is None" >> {
      val o: Option[Int] = None
      o.filter(v => v % 2 == 0) mustEqual None
    }
  }

  "4.2) Variance" >> {
    val s = Seq[Double](1, 5, 10, 20)
    "variance of sequence is Some(value)" >> {
      Option.variance(s) mustEqual Some(50.5)
    }
    "variance of empty sequence should pass through None" >> {
      Option.variance(Seq.empty[Double]) mustEqual None
    }
  }

  "4.3) lifting functions with map2" >> {
    "map2 will add two Option[Int]'s successfully" >> {
      Option.map2(Some(1), Some(1))(_ + _) mustEqual Some(2)
    }
    "map2 will short circuit to None when its passed to either param of map2" >> {
      Option.map2(None:Option[Int], Some(1))(_ + _) mustEqual None
      Option.map2(Some(1), None)(_ + _) mustEqual None
    }
  }
  "4.4) sequence of Option's" >> {
    "sequence will transform a Sequence of Some's to a Some[Seq[T]]" >> {
      Option.sequence(List(Some(1), Some(1))) mustEqual Some(Seq(1,1))
    }
    "sequence will transform a Sequence of Option's that contain a None to None" >> {
      Option.sequence(List(Some(1), None)) mustEqual None
    }
    "sequence will transform an empty Sequence to Some[List[T]]" >> {
      Option.sequence(List[Option[Int]]()) mustEqual Some(Nil)
    }
  }
  "4.5) traverse" >> {
    "traverse will convert and map a function over a given list" >> {
      Option.traverse(List(1,1))(x => Some(x + 1)) mustEqual Some(List(2,2))
    }
    "traverse will convert and map a function over a given list that contains a None and return None" >> {
      Option.traverse(List(1,1))(x => None) mustEqual None
    }
  }
}
