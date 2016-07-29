package repl.par

import java.util.concurrent.Executors
import parallelism.Par

object ParRepl {
  def main(args: Array[String]) = {
    println(sum(List(1,2,3,4)))
    println(sum2(IndexedSeq(1,2,3,4)))
  }

  def sum(ints: Seq[Int]): Int =
    ints.foldLeft(0)((a,b) => a + b)

  def sum2(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length/2)
      sum(l) + sum(r)
    }

  def sum3(ints: IndexedSeq[Int]): Par.Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length/2)
      Par.map2(Par.fork(sum3(l)), Par.fork(sum3(r)))((a,b) => a + b)
    }

  def sortedPar(ps: Par.Par[List[Int]]) = Par.map(ps)(l => l.sorted)

  def sum4(ints:IndexedSeq[Int]): Par.Par[Int] =
    Par.reduce(ints, 0)((a, b) => a + b)((b1, b2) => b1 + b2)

  def asyncSplit(): String => Par.Par[List[String]] = {
    Par.asyncF((p) => p.split(" ").toList)
  }

  def wordCount(pars: IndexedSeq[String])  = {
    Par.reduce(pars, 0)((a, b) => a.split(" ").length + b)((b1, b2) => b1 + b2)
  }

  def run[A](par: Par.Par[A]): A = {
    Par.run(Executors.newFixedThreadPool(2))(par)
  }

  def parMapExample(n: Int) = {
    val p = Par.parMap(List.range(1, n))(math.sqrt(_))
    val x = Par.run(Executors.newFixedThreadPool(2))(p)
    x
  }

  def safeRun[A](p: Par.SafePar[A]) = {
    println(Par.safeRun(Executors.newFixedThreadPool(2))(p))
  }

  def safeRunDemo(n: Int) = {
    val p = Par.safeMap(Par.safeUnit(1))(a => a / 0)
    safeRun(p)

    val q = Par.safeMap(Par.safeUnit(10))(a => a / 2)
    safeRun(q)

    val r = Par.safeLazyUnit({ throw new IllegalStateException("Exception thrown") })
    safeRun(r)

    val s = Par.safeMap2(q, r)((a, b) => (a, b))
    safeRun(s)

    val t = Par.safeMap2(q, q)((a, b) => a + b)
    safeRun(t)

    val u = Par.safeParMap(List.range(1, n))(math.sqrt(_))
    safeRun(u)
  }
}

/*
---- What tools exist for implementing the actual concurrency?

The primitives for implementing the lower levels are from java's concurrency lib

java.util.concurrent

---- Which primitives are available?

Interface ExecutorService
/** Submits a value returning task */
def submit(task: Callable[A]): Future[A]

Interface Callable
/** Computes a result */
def call[A](): A

Interface Future
/** Attempt to cancel execution of a task */
def cancel(evenIfRunning: Boolean): Boolean
/** Wait for a result */
def get(): V
/** Wait a specified timeout for a result */
def get(timeout: Long, timeUnit: TimeUnit): V
/** Determine if cancelled */
def isCancelled(): Boolean
/** Determine if done */
def isDone(): Boolean

So, in simple terms, a Callable needs to be created so that it can be submitted to
an ExecutorService. The ExecutorService will the produce a Future, which can be
observed for the result.

---- What type of executors are there?
http://docs.oracle.com/javase/7/docs/api/java/util/concurrent/Executors.html

newCachedThreadPool - Caches used threads for 60s so that quick short-lived tasks
 don't have as much overhead spinning up threads
newFixedThreadPool - Create a fixed size thread pool for tasks, threads exist and
 use resources even if they are not being used
newScheduledThreadPool - Allows tasks to be run after a given delay
newSingleThreadExecutor - Use a single worker thread with an unbounded queue
unconfigurableExecutorService - Allows a config freeze

*/
