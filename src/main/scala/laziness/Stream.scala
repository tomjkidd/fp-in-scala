package laziness

// NOTE: This import is the difference between being able to compile or not!
import scala.collection.immutable.{Stream => _}
import Stream._

trait Stream[+A] {
  
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  /** EXERCISE 5.1 */

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def toList2: List[A] = {
    @annotation.tailrec
    def go(as: Stream[A], acc: List[A]): List[A] = as match {
      case Cons(h, t) => go(t(), h() :: acc)
      case Empty => acc
    }

    go(this, List()).reverse
  }

  /** EXERCISE 5.2 */

  def take(n: Int): Stream[A] =  this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, t) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => t().drop(n - 1)
    case Cons(h, t) if n == 1 => t()
    case _ => empty
  }

  /** EXERCISE 5.3 */

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => if (p(h())) cons(h(), t().takeWhile(p)) else Empty
    case _ => Empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists2(p: A => Boolean): Boolean =
    foldRight(false)((cur, acc) => p(cur) || acc)

  /** EXERCISE 5.4 */

  def forall(p: A => Boolean): Boolean =
    foldRight(true)((cur, acc) => p(cur) && acc)

  /** EXERCISE 5.5 */

  def takeWhileUsingFoldRight(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((cur, acc) => if (p(cur)) cons(cur, acc) else Empty)

  /** EXERCISE 5.6 */

  def headOptionUsingFoldRight: Option[A] =
    foldRight[Option[A]](None)((cur, acc) => Some(cur))

  /** EXERCISE 5.7 */

  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](Empty)((cur, acc) => cons(f(cur), acc))

  def filter(f: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((cur, acc) => if(f(cur)) cons(cur, acc) else acc)

  def append[B >: A](bs: => Stream[B]): Stream[B] =
    foldRight[Stream[B]](bs)((cur, acc) => cons(cur, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](Empty)((cur, acc) => f(cur).append(acc))

  def tail(): Stream[A] =
    foldRight[(Stream[A], Stream[A])]((empty, empty))((cur, result) => result match {
      case (f, s) => (s, cons(cur, s))
    })._1

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  /** EXERCISE 5.13 */

  def mapUnfold[B](f: A => B): Stream[B] =
    unfold[B, Stream[A]](this)((rem) => rem match {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    })

  def takeUnfold(n: Int): Stream[A] =
    unfold[A, (Int, Stream[A])]((n, this))(s  => s match {
      case (m, Cons(h, t)) if m > 1 => Some( (h(), (m - 1, t())) )
      case (m, Cons(h, t)) if m == 1 => Some( (h(), (0, Empty)) )
      case _ => None
    })

  def takeWhileUnfold(p: A => Boolean): Stream[A] =
    unfold[A, Stream[A]](this)(s => s match {
      case Empty => None
      case Cons(h, t) => if (p(h())) Some( h(), t() ) else None
    })

  def zipWithUnfold[B, C](ys: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold[C, (Stream[A], Stream[B])]((this, ys))(s => s match {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(ah, at), Cons(bh, bt)) => Some( (f(ah(), bh()), (at(), bt())) )
    })

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold[(Option[A], Option[B]), (Stream[A], Stream[B])]((this, s2))(s => s match {
      case (Empty, Cons(bh, bt)) => 
        Some(
          (None, Some(bh())),
          (Empty, bt()))
      case (Cons(ah, at), Empty) => 
        Some(
          (Some(ah()), None),
          (at(), Empty))
      case (Cons(ah, at), Cons(bh, bt)) =>
        Some(
          (Some(ah()), Some(bh())),
          (at(), bt()))
      case _ => None
    })

  /** EXERCISE 5.14 */

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).filter(z => z match {
      case (_, None) => false
      case _ => true
    }).forall(z => z match {
      case (Some(x), Some(y)) => x == y
      case _ => false
    })

  /** EXERCISE 5.15 */

  def tails: Stream[Stream[A]] =
    unfold[Stream[A], Stream[A]](this)(s => s match {
      case Cons(h, t) => Some( (cons(h(),t()), t()) )
      case Empty => None
    }).append(Stream(empty))

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (x => x.startsWith(s))

  /** EXERCISE 5.16 */

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight[(B, Stream[B])]((z, Stream(z)))((a, tup) => {
      lazy val ltup = tup
      val b = f(a, ltup._1)
      val acc = ltup._2
      (b, cons(b, acc))
    })._2

  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWithUnfold(s2)((_,_))

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
 
  def cons[B](hd: => B, tl: => Stream[B]): Stream[B] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  /** EXERCISE 5.8 */

  def constant[A](a: A): Stream[A] =
    Stream.cons(a, constant(a))

  /** EXERCISE 5.9 */

  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n + 1))

  /** EXERCISE 5.10 */

  def fib(n: Int) : Int = {
    if (n <= 0) 0
    else if (n == 1) 1
    else fib(n-1) + fib(n-2)
  }

  def fibs(n: Int): Stream[Int] =
    Stream.cons(fib(n), fibs(n + 1))

  /** EXERCISE 5.11 */
  
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
     f(z) match {
       case None => Empty
       case Some((a, s)) =>
         Stream.cons(a, unfold(s)(f))
     }
  }

  /** EXERCISE 5.12 */

  def fibsUnfold(): Stream[Int] =
    unfold((0, 1))(s => s match {
      case (f1, f2) => Some(f1 , (f2, f1 + f2))
    })

  def fromUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some(n, n + 1))

  def constantUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some(n, n))

  def onesUnfold(): Stream[Int] =
    unfold(1)(_ => Some(1, 1))

  // TODO: Figure out why this compiles but does not work...
  def mapUnfold2[A,B](as: Stream[A])(f: A => B): Stream[B] =
    unfold(as)((rem) => rem match {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    })
}
