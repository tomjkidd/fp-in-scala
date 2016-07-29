package testing

import state._
import rng._

import Prop._
import laziness._

// State[RNG, A] eq RNG => (A, RNG)
case class Gen[+A](sample: State[RNG, A]) {

  /* EXERCISE 8.6 */

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen[B](sample.flatMap(s => f(s).sample))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => this.listOfN(n))

  def map[B](f: A => B): Gen[B] =
    flatMap(s => Gen.unit(f(s)))

  /* EXERCISE 8.10 */

  def unsized: SGen[A] =
    SGen(_ => this)
}

case class SGen[+A](forSize: Int => Gen[A]) {
  
  /* EXERCISE 8.11 */

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen((n) => forSize(n).flatMap(a =>
      f(a).forSize(n)))

  def map[B](f: A => B): SGen[B] =
    SGen((n) => forSize(n).map(a => f(a)))

  /* EXERCISE 8.12 */

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen((size) => {
      g.listOfN(size)
    })
}

object Gen {
  /*def listOf[A](a: Gen[A]): Gen[List[A]]

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]]*/

  /* EXERCISE 8.4 */

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen[Int](rng.RNG.intBetween(start, stopExclusive))

  /* EXERCSE 8.5 */

  def unit[A](a: => A): Gen[A] =
    Gen[A](state.State.unit(a))

  def boolean: Gen[Boolean] =
    Gen[Boolean](rng.RNG.booleanState)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen[List[A]](state.State.sequence(List.fill(n)(g.sample)))

  /* EXERCISE 8.7 */

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  /* EXERCISE 8.8 */

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val total = g1._2 + g2._2
    val threshold = g1._2/total 
    Gen(rng.RNG.doubleState.flatMap(n =>
      if (n < threshold) g1._1.sample
      else g2._1.sample))
  }

}


/*trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def &&(p: Prop): Prop
}*/
case class Prop(run: (TestCases,RNG) => Result) {
  /* EXERCISE 8.9 */

  def &&(p: Prop): Prop =
    Prop((tc, rng) => {
      val r = this.run(tc, rng)
      r match {
        case Passed => p.run(tc, rng)
        case f => f
      }
    })

  def ||(p: Prop): Prop =
    Prop((tc, rng) => {
      val r = this.run(tc, rng)
      r match {
        case Passed => Passed
        case _ => p.run(tc, rng)
      }
    })
}

object Prop {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase,
    successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map({
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }).find(x => x.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}
