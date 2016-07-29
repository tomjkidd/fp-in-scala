package testing

import java.util.concurrent.{ExecutorService, Executors}

import state._
import rng._

import Prop._
import laziness._
import parallelism._

// State[RNG, A] eq RNG => (A, RNG)
/** A class used to generated random samples */
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

  def map2[B,C](gb: Gen[B])(f: (A,B) => C): Gen[C] =
    flatMap[C](a => {
      gb.flatMap[C](b => {
        Gen.unit(f(a,b))
      })
    })

  /** Combine this Gen with another in a tuple */
  def **[B](g: Gen[B]): Gen[(A,B)] =
    (this map2 g)((a, b) => (a, b))

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

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))

  /* EXERCISE 8.13 */

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n max 1))
}


/*trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def &&(p: Prop): Prop
}*/
case class Prop(run: (MaxSize,TestCases,RNG) => Result) {
  /* EXERCISE 8.9 */

  def &&(p: Prop): Prop =
    Prop((max, tc, rng) => {
      val r = this.run(max, tc, rng)
      r match {
        case Passed => p.run(max, tc, rng)
        case Proved => p.run(max, tc, rng)
        case f => f
      }
    })

  def ||(p: Prop): Prop =
    Prop((max, tc, rng) => {
      val r = this.run(max, tc, rng)
      r match {
        case Passed => r
        case Proved => r
        case _ => p.run(max, tc, rng)
      }
    })
}

object Prop {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Proved extends Result {
    def isFalsified = true
  }

  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase,
    successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => {
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop((max, _, rng) => p.run(max, casesPerSize, rng))).toList.reduce(_ && _)
      prop.run(max, n, rng)
    }
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"!Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  def check(p: Boolean): Prop =
    Prop((_,_,_) => if (p) Passed else Falsified("()", 0))

  /* EXERCISE 8.15 */

  /*
   * Small domains can be Proved, as opposed to just Passed
   * Boolean, Byte, and other small domains could be added
   */
  // TODO: Add a function to prove Byte type

  def forAllBool(f: Boolean => Boolean): Prop =
    Prop((max,n,rng) => {
      val tGen = Gen.unit(true)
      val fGen = Gen.unit(false)
      val tfGen = tGen.map2[Boolean, Boolean](fGen)((a, b) => f(a) && f(b))
      forAll(tfGen)(f).run(max, 1, rng) match {
        case Passed => Proved
        case v => v
      }
    })

  def S = Gen.weighted(
    Gen.choose(1,4).map(Executors.newFixedThreadPool) -> .75,
    Gen.unit(Executors.newCachedThreadPool) -> .25)

  def forAllPar[A](g: Gen[A])(f: A => Par.Par[Boolean]): Prop =
    forAll(S.map2(g)((_,_))){ case (s,a) => Par.run(s)(f(a)) }

  def forAllPar2[A](g: Gen[A])(f: A => Par.Par[Boolean]): Prop =
    forAll(S ** g){ case (s,a) => Par.run(s)(f(a))}

  def checkPar(p: Par.Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)
    
}
