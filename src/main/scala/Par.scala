package parallelism

import java.util.concurrent.{ExecutorService, Callable, CountDownLatch}
import java.util.concurrent.atomic.{AtomicReference}

sealed trait Future[+A] {
  private[parallelism] def apply(k: A => Unit): Unit
}

/** A set of functions to describe and evaluate parallel computations
  * 
  */
object Par {

  type Par[+A] = ExecutorService => Future[A]

  /** Turn a value into a computation that immediately is available
    *
    * @param a  The raw value to turn into a Par[A]
    * @tparam A The type of the raw value
    */
  def unit[A](a: A):Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        cb(a)
    }

  /** Turn a value into a lazy computation, mark for concurrent evaluation */
  def lazyUnit[A](a: => A):Par[A] = fork(unit(a))

  /* EXERCISE 7.1 */

  /** Combine the results of two parallel compuations */
  def map2[A,B,C](p: Par[A], p2:Par[B])(f: (A, B) => C): Par[C] =
    es => new Future[C] {
      def apply(cb: C => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None

        val combiner = parallelism.ref.Actor[Either[A,B]](es) {
            case Left(a) =>
              if (br.isDefined) eval(es)(cb(f(a,br.get)))
              else ar = Some(a)
            case Right(b) =>
              if (ar.isDefined) eval(es)(cb(f(ar.get,b)))
              else br = Some(b)
          }

        p(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }
    }

  /*def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))*/

  // specialized version of `map`
  def map[A,B](p: Par[A])(f: A => B): Par[B] =
    es => new Future[B] {
      def apply(cb: B => Unit): Unit =
        p(es)(a => eval(es) { cb(f(a)) })
    }

  /** Mark a computation for concurrent evaluation */
  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        eval(es)(a(es)(cb))
    }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = r })

  /*def deadlockingFork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })*/

  /* EXERCISE 7.9 */

  /*
   For the newFixedThreadPool(1) case, there is a single thread.
   In this case, es.submit is called with a(es).get.
   a: ExecutorService => Future[A], therefore a(es) is a Future[A]
   a.get is a call to Future.get, which will block until our result is ready
   Because we are already on the only thread, this will deadlock.

   For the newFixedThreadPool(n) case, there are n threads.
   The only difference with the above is that there are some cases where
   we won't deadlock, but a deadlock is still possible with n computations.
   */

  /** Evaluate a parallel computation */
  def run[A](es: ExecutorService)(p: Par[A]): A = {

    val ref = new AtomicReference[A]

    val latch = new CountDownLatch(1)

    p(es) { a => ref.set(a); latch.countDown }

    latch.await

    ref.get
  }

  /** EXERCISE 7.10 */

  def safeRun[A](es: ExecutorService)(p: Par[A]): Option[A] = {
    val ref = new AtomicReference[Option[A]]
    val latch = new CountDownLatch(1)

    // map, map2, and fork make calls to eval, which call es.submit with a Callable.
    // submit can fail with RejectedExecutionException
    // Callable's call can fail with Exception

    try {

      p(es) { a => ref.set(Some(a)) }

    } catch {

      case t: Throwable => ref.set(None)

    } finally {

      latch.countDown

    }
    
    latch.await

    ref.get
  }

  /* EXERCISE 7.5 */

  // NOTE: This will lead to stack overflows for large calculations!
  def sequenceThatStackOverflows[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(Nil))((cur, acc) =>
      map2(cur, acc)((h,t) => h :: t))

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l,r) = as.splitAt(as.length/2)
        map2(sequenceBalanced(l), sequenceBalanced(r))((ls, rs) => ls ++ rs)
      }
    }

  def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  /* Exercise 7.4 */

  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = {
    sequence(ps.map(asyncF(f)))
  }

  /* EXERCISE 7.6 */

  def parFilterNaive[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    parMap(as.filter(f))(identity)

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    // Map over pars, replace filtered out with empty list
    val listOfListPar = as.map(asyncF((a) => if (f(a)) List(a) else List()))

    // Apply sequence to combine to list of lists within a single par
    val parOfListOfLists = sequence(listOfListPar)

    // Use flatten to remove the empty lists and concat the results
    val parOfList = map(parOfListOfLists)(x => x.flatten)

    parOfList
  }

  def reduce[A,B](as: IndexedSeq[A], b: B)(f: (A, B) => B)(g: (B, B) => B): Par[B] =
    if (as.size <= 1) {
      as.headOption match {
        case None => unit(b)
        case Some(a) => unit(f(a,b))
      }
    }
    else {
      var (l, r) = as.splitAt(as.length/2)
      Par.map2(
        Par.fork(reduce(l, b)(f)(g)),
        Par.fork(reduce(r, b)(f)(g)))((cur, acc) => g(cur, acc))
    }

  def map3[A,B,C,D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A,B,C) => D) = {
    val ab = map2(pa, pb)((a, b) => (a, b))
    map2(ab, pc)((ab, c) => ab match { case (a, b) => f(a, b, c) })
  }

  /*def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e) == p2(e)*/

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)
}

/* EXERCISE 7.7 */

/*
Given:
1. map(y)(id) == y
2. map(y)(g) == g(y) //using 1.

Prove:
[1]map(map(y)(g))(f) == map(y)(f compose g)
[2]map(g(y))(f) == map(y)(f compose g) //using 2.
[3]f(g(y)) == map(y)(f compose g) //using 2.
[4]f(g(y)) == (f compose g)(y) //Using 2.
[5]f(g(y)) == f(g(y)) // Using def of compose
 */
