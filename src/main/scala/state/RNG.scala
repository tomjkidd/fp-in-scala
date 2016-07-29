package rng

import state._
import state.State._

/*
 * State[S, +A](run: S => (A,S)
 * In the context of this file, S is RNG
 */

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def int(rng: RNG): (Int, RNG) = {
    rng.nextInt
  }

  def intUsingState(rng: RNG): (Int, RNG) = {
    state.State[RNG, Int](r => r.nextInt).run(rng)
  }

  def nonNegativeIntUsingState(rng: RNG): (Int, RNG) = {
    state.State[RNG, Int](r => r.nextInt).map(n =>
      if (n == Int.MinValue) 0
      else Math.abs(n: Int)).run(rng)
  }

  def intState: state.State[RNG, Int] = 
    state.State[RNG, Int](r => r.nextInt)

  def doubleState: state.State[RNG, Double] =
    nonNegativeIntState.map(n => n.toDouble / (Int.MaxValue.toDouble + 1))

  def booleanState: state.State[RNG, Boolean] =
    intState.map(n => n % 2 == 0)

  def trueState: State[RNG, Boolean] =
    intState.map(_ => true)

  def falseState: State[RNG, Boolean] =
    intState.map(_ => false)

  def nonNegativeIntState: State[RNG, Int] =
    intState.map(n => if (n == Int.MinValue) 0 else Math.abs(n))

  def nonNegativeLessThanState(n: Int): State[RNG, Int] =
    nonNegativeIntState.flatMap(i => {
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        state.State.unit(mod)
      else
        nonNegativeLessThanState(n)
    })

  /* d is normalized between 0 and 1, exclusive
   * If we divide this range into (stopExclusive - start) pieces,
   * We can use mod division to get a 0 to (stopExclusive - start) value
   * Adding start to this value will put you into the right range
   */
  def intBetween(start: Int, stopExclusive: Int): State[RNG, Int] =
    doubleState.map(d => {
      val spread = stopExclusive - start
      val steps = Int.MaxValue + 1
      val scaled = (spread * d).toInt
      scaled + start
    })

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    if (n == Int.MinValue) (0, rng2)
    else (Math.abs(n: Int), rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = nonNegativeInt(rng)
    val d = (n.toDouble / (Int.MaxValue.toDouble + 1))
    (d, rng2)
  }

  /*def nonNegativeLessThan(rng: RNG, n: Int): (Int, RNG) = {
    state.State[RNG, Int](
  }*/
}
