package fpinscala.errorhandling

import org.specs2.mutable.Specification

import scala.{Either => _, Left => _, Option => _, Right => _, Some => _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

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
  "4.6) Either implementation" >> {
    "map over right of Either" >> {
      Right(1).map(_ + 1) mustEqual Right(2)
    }
    "flatMap over right of Either" >> {
      Right(1).flatMap(v => Right(v + 1)) mustEqual Right(2)
    }
    "orElse returns another value when Either is Left" >> {
      Left("Error").orElse(Right(1)) mustEqual Right(1)
    }
    "map2 applies a function on two Right's and returns a Right" >> {
      Right(1).map2(Right(2))(_ + _) mustEqual Right(3)
    }
    "map2 applies a function on one Right and one Left and returns a Left" >> {
      Right(1).map2(Left("Error"))(_ + _) mustEqual Left("Error")
      Left("Error").asInstanceOf[Either[String, Int]].map2(Right(1))(_ + _) mustEqual Left("Error")
    }
  }
  "4.7) Either sequence and traversal impl." >> {
    "sequence will transform a Sequence of Either Right's to a Right[Seq[T]]" >> {
      Either.sequence(List(Right(1), Right(2))) mustEqual Right(Seq(1, 2))
    }
    "sequence will transform a Sequence of Either's that contain a Left to Left" >> {
      Either.sequence(List[Either[String, Int]](Right(1), Left("Ahh!"))) mustEqual Left("Ahh!")
    }
    "traverse will convert and map a function over a given list" >> {
      Either.traverse(List(1,1))(x => Right(x + 1)) mustEqual Right(List(2,2))
    }
    "traverse will convert and map a function over a given list that contains a None and return None" >> {
      Either.traverse(List(1,1))(x => Left("Ahh!")) mustEqual Left("Ahh!")
    }
  }
  "4.8) Either that can contain more than 1 error" >> {
    /*
    Either implementation would need to change so that when pattern matching Left[Seq[String]]
    it would append to a Left value.  The implementation right now currently only passes the
    first Left accumulated as the result.

    Scalatic's Or implementation supports accumulation of errors.  The type signature would look
    like: Person Or Every[ErrorMessage]
     */
    success
  }
}
