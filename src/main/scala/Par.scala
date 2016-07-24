package parallelism

import java.util.concurrent.{ExecutorService, Future, Callable, TimeUnit}

/** A set of functions to describe and evaluate parallel computations
  * 
  */
object Par {

  type Par[A] = ExecutorService => Future[A]

  /** Turn a value into a computation that immediately is available
    *
    * @param a  The raw value to turn into a Par[A]
    * @tparam A The type of the raw value
    */
  def unit[A](a: A):Par[A] = (es: ExecutorService) => UnitFuture(a)

  /** Turn a value into a lazy computation, mark for concurrent evaluation */
  def lazyUnit[A](a: => A):Par[A] = fork(unit(a))

  /* EXERCISE 7.1 */

  /** Combine the results of two parallel compuations */
  def map2[A,B,C](a: Par[A], b:Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      Map2Future(af, bf, f)
    }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  /** Mark a computation for concurrent evaluation */
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def deadlockingFork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

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
  //def run[A](s: ExecutorService)(a: Par[A]): Future[A]

  /* EXERCISE 7.5 */

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(Nil))((cur, acc) =>
      map2(cur, acc)((h,t) => h :: t))

  /* Exercise 7.4 */

  def asyncF[A,B](f: A => B): A => Par[B] =
    (a) => lazyUnit(f(a))

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs = ps.map(asyncF(f))
    sequence(fbs)
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
        Par.fork(reduce(l, b)(f)(g)))((cur, acc) => g(cur, acc))
    }

  def map3[A,B,C,D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A,B,C) => D) = {
    val ab = map2(pa, pb)((a, b) => (a, b))
    map2(ab, pc)((ab, c) => ab match { case (a, b) => f(a, b, c) })
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  /** Define at low-level how to return a Future for a known value */
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  /* EXERCISE 7.3 */

  /** Define at low-level how to combine two futures */
  private case class Map2Future[A,B,C](a: Future[A], b: Future[B], f: (A,B) => C) extends Future[C] {
    // Cache is a variable for the result
    @volatile var cache: Option[C] = None

    def isDone = cache.isDefined

    def get = compute(Long.MaxValue)
    def get(timeout: Long, units:TimeUnit) =
      compute(TimeUnit.NANOSECONDS.convert(timeout, units))

    def isCancelled = a.isCancelled || b.isCancelled

    def cancel(evenIfRunning: Boolean): Boolean = 
      a.cancel(evenIfRunning) || b.cancel(evenIfRunning)

    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(c) => c
      case None => {
        // The timeout needs to be shared...
        val aStart = System.nanoTime
        val aResult = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val aEnd = System.nanoTime

        val aDuration = (aEnd - aStart)

        // The reamaining time that future b has to run
        val bLimit = timeoutInNanos - aDuration

        val bResult = b.get(bLimit, TimeUnit.NANOSECONDS)

        val result = f(aResult, bResult)

        cache = Some(result)

        result
      }
    }
  }
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
