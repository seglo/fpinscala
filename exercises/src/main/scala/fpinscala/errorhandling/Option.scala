package fpinscala.errorhandling


import scala.{Either => _, Option => _, Some => _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case _ => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(a) => a
    case _ => default
  }

  // Q, how does f fail?  "Apply f , which may fail, to the Option if not None ."
  def flatMapMatching[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case _ => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def orElseMatching[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    this.map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = this.flatMap { a =>
    if (f(a)) Some(a)
    else None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap { m =>
      mean(xs.map(x => math.pow(x - m, 2)))
    }


  def map2_nested[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(a1 => b.flatMap(b1 => Some(f(a1, b1))))
  }

  // Q, how does for know when to call map or flatMap?
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
    a1 <- a
    b1 <- b
  } yield f(a1, b1)

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h.flatMap(h => sequence(t).map(_ ++ Seq(h)))
    }


  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h :: t =>
        val applied = f(h)
        applied.flatMap { v =>
          traverse(t)(f).map( _ ++ Seq(v))
        }
    }

}