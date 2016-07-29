package parallelism.actor

import java.util.concurrent.{ExecutorService, Callable}
import java.util.concurrent.atomic.{AtomicReference, AtomicInteger}
import annotation.tailrec

/** An implementation of the Actor pattern used to support definition in Par
  * 
  */

/*
 New objects...

 Class AtomicReference - Object that can be updated atomically
 def compareAndSet
 def get
 def getAndSet
 def lazySet
 def set
 def toString
 def weakCompareAndSet

 Class AtomicInteger
 tldr - A bunch of methods to handle inc/dec/swap type of changes
 def set
 def compareAndSet
 */

trait Strategy {
  /** A function that defers retreiving a */
  def apply[A](a: => A): () => A
}

object Strategy {
  def fromExecutorService(es: ExecutorService): Strategy = new Strategy {
    def apply[A](a: => A): () => A = {
      val f = es.submit(new Callable[A] { def call = a })
      () => f.get
    }
  }

  def sequential: Strategy = new Strategy {
    def apply[A](a: => A): () => A = {
      val r = a
      () => r
    }
  }
}

private class Node[A](var a: A = null.asInstanceOf[A]) extends AtomicReference[Node[A]]

final class Actor[A](strategy: Strategy)(handler: A => Unit, onError: Throwable => Unit = throw(_)) {
  self => 
  private val tail = new AtomicReference(new Node[A]())
  private val suspended = new AtomicInteger(1)
  private val head = new AtomicReference(tail.get)

  def !(a: A) = {
    val n = new Node(a)
    head.getAndSet(n).lazySet(n)
    trySchedule()
  }

  def apply(a: A) = {
    this ! a
  }

  def contramap[B](f: B => A): Actor[B] =
    new Actor(strategy)((b: B) => (this ! f(b)), onError)

  private def trySchedule(): Unit = {
    // The compareAndSet will return true when 1 was seen, and 0 was then written
    if (suspended.compareAndSet(1, 0)) schedule()
  }

  private def schedule(): Unit = {
    strategy(act())
  }

  private def act() = {
    val t = tail.get
    val n = batchHandle(t, 16384)
    if (n ne t) {
      n.a = null.asInstanceOf[A]
      tail.lazySet(n)
      schedule()
    } else {
      suspended.set(1)
      if (n.get ne null) trySchedule()
    }
  }

  @tailrec
  private def batchHandle(t: Node[A], i: Int): Node[A] = {
    val n = t.get
    if (n ne null) {
      try {
        handler(n.a)
      } catch {
        case ex: Throwable => onError(ex)
      }
      if (i > 0) batchHandle(n, i - 1) else n
    } else t
  }
}

object Actor {

  /** Create an `Actor` backed by the given `ExecutorService`. */
  def apply[A](es: ExecutorService)(handler: A => Unit, onError: Throwable => Unit = throw(_)): Actor[A] =
    new Actor(Strategy.fromExecutorService(es))(handler, onError)
}
