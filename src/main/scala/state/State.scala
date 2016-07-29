package state

import State._

case class State[S, +A](run: S => (A,S)) {
  /** EXERCISE 6.10 */

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, sB) = run(s)
      f(a).run(sB)
    })

  def map[B](f: A => B): State[S, B] =
    flatMap[B](a => {
      unit(f(a))
    })

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap[C](a => {
      sb.flatMap[C](b => {
        unit(f(a,b))
      })
    })
}

object State {

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight[State[S, List[A]]](unit(Nil))((cur, acc) =>
      cur.map2(acc)((h,t) => h :: t))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def modify2[S](f: S => S): State[S, Unit] =
    // State[S, Unit] maps to a fn, s => (a, s)
    // modify2 should return a fn with s => ((), s)
    State((s:S) => {
      val (targetState,_) = get.run(s)
      val newState = f(targetState)
      val modifiedState = set(targetState).run(newState)
      modifiedState
    })

  def modify3[S](f: S => S): State[S, Unit] =
    State((s: S) => {
      get.flatMap((targetState: S) =>
        set(f(targetState)).flatMap(u =>
          unit(u))).run(s)
    })

  def modify4[S](f: S => S): State[S, Unit] =
    get.flatMap((targetState: S) =>
      set(f(targetState)).flatMap(u =>
        unit(u)))

  def modify5[S](f: S => S): State[S, Unit] =
    for {
      targetState <- get
      u <- set(f(targetState))
    } yield u
}
