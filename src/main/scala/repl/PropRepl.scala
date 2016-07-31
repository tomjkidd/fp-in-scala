package repl.prop

import java.util.concurrent.{ExecutorService, Executors}
import parallelism._
import laziness._

import testing._
import testing.Gen._
import testing.Prop._

import state._
import rng._

object PropRepl {
  def example1() = {
    val smallInt = Gen.choose(-10,10)
    val maxProp = forAll(listOf1(smallInt))((ns) => {
      val max = ns.max
      !ns.exists(_ > max)
    })

    Prop.run(maxProp)
  }

  /* EXERCISE 8.14 */

  def exercise8Dot14() = {
    val intGen = Gen.choose(-100, 100)
    val sortProp = forAll(listOf1(intGen))((ns) => {
      val sortedNs = ns.sorted.toList
      // Start with the first element, and check that forall subsequent
      // elements that they remain in order
      sortedNs.foldLeft[Option[Int]](
        Some(sortedNs.head)
      )((oy: Option[Int], x:Int) => oy match {
        case None => None
        case Some(y) => if (x >= y) Some(x) else None
      }).isDefined
    })

    Prop.run(sortProp)
  }

  def evolveTestingPar() = {
    val ES: ExecutorService = Executors.newCachedThreadPool

    val propTest1 = Prop.forAll(Gen.unit(Par.unit(1)))(i =>
      Par.run(ES)(Par.map(i)(x => x + 1)) == Par.run(ES)(Par.unit(2)))

    Prop.run(propTest1)

    val propTest2 = Prop.check {
      val p = Par.map(Par.unit(1))(x => x + 1)
      val p2 = Par.unit(2)
      Par.run(ES)(p) == Par.run(ES)(p2)
    }

    val propTest3 = Prop.check {
      val par = Par.equal(
        Par.map(Par.unit(1))(x => x + 1),
        Par.unit(2))
      Par.run(ES)(par)
    }

    val propTest4 = Prop.checkPar {
      Par.equal(
        Par.map(Par.unit(1))(x => x + 1),
        Par.unit(2))
    }

    val pint = Gen.choose(0,10).map(x => Par.unit(x))
    val propTest5 =
      Prop.forAllPar(pint)(n => Par.equal(Par.map(n)(y => y), n))
  }

  def exercise8Dot15() = {
    Prop.run(Prop.forAllBool(p => (p && !p) == false))
    Prop.run(Prop.forAllBool(p => (p && p) == p))
    Prop.run(Prop.forAllBool(p => (p || !p) == true))
  }

  /* EXERCISE 8.16 */

  val rng1 = rng.RNG.Simple(System.currentTimeMillis)
  val es1 = Executors.newCachedThreadPool

  val intOps: Gen[(Int,Int) => Int] =
    weighted(
      unit((a:Int, b:Int) => a + b) -> .50,
      unit((a:Int, b:Int) => a - b) -> .50)

  val parUnitOps: Gen[Int => Par.Par[Int]] =
    weighted(
      unit((a: Int) => Par.unit(a)) -> .25,
      unit((a: Int) => Par.lazyUnit(a)) -> .75)

  val intStream:Stream[Int] = Prop.randomStream(Gen.choose(0,10))(rng1)

  val opStream:Stream[(Int,Int) => Int] =
    Prop.randomStream(intOps)(rng1)

  val unitStream:Stream[Int => Par.Par[Int]] =
    Prop.randomStream(parUnitOps)(rng1)

  def richPar: Gen[Par.Par[Int]] = {
    /*
     * Which components do we have to work with?
     * Par.{unit, fork, lazyUnit, map2, map}
     */
    // unit[A](a: A):Par[A]
    // lazyUnit[A](a: A):Par[A]
    // map2[A,B,C](p: Par[A], p2: Par[B])(f:(A,B) => C): Par[C]
    // map[A,B](p: Par[A])(f: A => B): Par[B]
    // fork[A](a: => Par[A]): Par[A]
    // Generate a list of ints
    // Choose unit or lazyUnit to map over them
    // Choose a random math function to combine them all down

    val int = Gen.listOf1(Gen.choose(0,10))
    Gen.choose(0,10).map(n => Par.unit(n))
  }

  def richParGen: Gen[Par.Par[Int]] = Gen(State((rng:RNG) => {
    val (elementsToCombine, rng2) = Gen.choose(0, 100).sample.run(rng)
    val (ints, rng3) = Gen.choose(-10, 10).
      listOfN(elementsToCombine).
      sample.run(rng2)

    val (unitFns, rng4) = Gen.oneOf(List(
      Gen.unit((n:Int) => Par.unit(n)),
      Gen.unit((n:Int) => Par.lazyUnit(n)))).
      listOfN(elementsToCombine).
      sample.run(rng3)

    val (ops, rng5) = Gen.oneOf(List(
      Gen.unit((a:Int,b:Int) => a + b),
      Gen.unit((a:Int,b:Int) => a - b))).
      listOfN(elementsToCombine - 1).
      sample.run(rng4)

    val richPar = ints.zip(unitFns).
      map((tup) => tup match {
        case (i, u) => u(i)
      }).
      zip(ops).
      foldLeft(Par.unit(0))((accPar, c) => c match {
        case (curPar, op) =>
          Par.map2(curPar, accPar)((cur, acc) => op(acc, cur))
      })

    (richPar, rng5)
  }))

  def exercise8Dot16() = {
    /*
     * The code presented here is a proof-of-concept for creating a rich Par
     * generator.
     * It would need to be refactored to be used by Gen directly
     */

    // TODO: Refactor this code so that it is a better generator (use more RNGs)
    // TODO: Make it so that take is variable so that the computation varies
    val richPar: Par.Par[Int] =
      PropRepl.intStream.zip(PropRepl.unitStream).
        map((tup) => tup match {
          case (i, u) => u(i)
        }).
        zip(PropRepl.opStream).
        take(10).toList.
        foldLeft(Par.unit(0))((accPar, c) => c match {
          case (curPar, op) =>
            Par.map2(curPar, accPar)((cur,acc) => op(acc,cur))
        })

    val richParGenLocal: Gen[Par.Par[Int]] = Gen.unit(richPar)
    val prop = Prop.forAllPar(richParGenLocal)(n => Par.equal(Par.fork(n), n))

    val prop2 = Prop.forAllPar(richParGen)(n => Par.equal(Par.fork(n), n))
    Prop.runAll(List(prop, prop2))

  }

  def exercis8Dot17() = {
    val pint = Gen.choose(0,10).map(x => Par.unit(x))
    val prop =
      Prop.forAllPar(pint)(n => Par.equal(Par.fork(n), n))

    Prop.run(prop)
  }

  val isEven = (i: Int) => i % 2 == 0

  def exercise8Dot18() = {
    
    val p = Prop.forAll(Gen.listOf(Gen.choose(-100, 100)))((ns) => {
      val taken = ns.takeWhile(isEven)
      val notTaken = ns.dropWhile(isEven)
      ns == (taken ++ notTaken)
    })

    val p2 = Prop.forAll(Gen.listOf(Gen.choose(-100, 100)))((ns) => {
      ns.takeWhile(isEven).forall(isEven)
    })

    val p3 = Prop.forAll(Gen.listOf(Gen.choose(-100, 100)))((ns) => {
      ns.startsWith(ns.takeWhile(isEven)) == true
    })

    Prop.runAll(List(p,p2,p3))
  }

  def exercise8Dot19() = {
    /*
     * String functions...
     * hashCode(): Int -> Depends on whole string
     * length()/size(): Int -> Less information, depends only on length
     * toInt(): Int -> Throws exceptions
     */
    
    def getStringIntFn(g: Gen[Int]): Gen[String => Int] =
      g map (i => (s => s.length))

    def getStringFn[A](g: Gen[A]): Gen[String => A] =
      Gen(State((rng: RNG) => {
        val (seed, rng2) = rng.nextInt
        val f = (s: String) => {
          g.sample.run(RNG.Simple(seed.toLong ^ s.hashCode.toLong))._1
        }
        (f, rng2)
       }))
  }

  def exercise8Dot20() = {
    /*
     * List and Stream props
     * for {take, drop, filter, unfold}
     *
     * Sized generator for Tree data type from Chapter 3
     * Specify the behavior of fold, improve the api
     *
     * Properties for sequence for Option and Either
     */
    // TODO: Create some properties for the above comment
    val smallInt = Gen.choose(-100, 100)
    val p = forAll(Gen.listOf(smallInt))((ns) => {
      val taken = ns.take(2)
      val notTaken = ns.drop(2)
      taken.length + notTaken.length == ns.length
    })

    val p2 = forAll(Gen.listOf(smallInt))((ns) => {
      val even = ns.filter(isEven)
      val odd = ns.filter(x => !isEven(x))
      even.length + odd.length == ns.length
    })

    Prop.runAll(List(p,p2))
  }
  
  def mapLaw() = {
    /*
     * Properties for Stream, List, Option, State
     * map(x)(id) == x
     */
  }

  def oneOfNaive[A](lgs: List[Gen[A]]): Gen[A] = {
    // NOTE: This does not give each element equal probability
    lgs.reduce((a, b) => Gen.weighted((a, .5),(b, .5)))
  }

  def testSplit() = {
    val rawChoices = List(1,2,3,4)
    val choices = rawChoices.map(n => Gen.unit(n))
    val generator = Gen.oneOf(choices)
    val p = forAll(generator)((n) => {
      rawChoices.contains(n)
    })

    Prop.run(p)
  }
}
