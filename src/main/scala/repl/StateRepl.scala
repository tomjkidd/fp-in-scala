package repl.state

import state.State._

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  
  type CandyMachine = state.State[Machine, (Int, Int)]

  // S maps to Machine
  // A maps to (Int, Int) where (# coins in machine, # candies in machine)
  // State[Machine, (Int, Int)] run: Machine => ((Int,Int), Machine)
  def init(): Machine = {
    val m = Machine(true, 10, 0)
    val s = state.State((m: Machine) => m match {
      case Machine(l, candies, coins) => ((coins, candies), m)
    })

    val result = state.State.modify(modifyHelper(Coin)).run(m)._2
    result
  }

  def simulateMachine(inputs: List[Input]): state.State[Machine, (Int, Int)] = {
    val is = inputs.map(i => state.State.modify(modifyHelper(i)))
    val simulation = state.State.sequence(is)
    simulation.flatMap(_ => get).map(m => m match {
      case Machine(_, candies, coins) => (coins, candies)
    })
  }

  def cycle[A](as: List[A], n: Int): List[A] =
    Stream.continually(as.toStream).flatten.take(n).toList

  def testSim() = {
    val inputs = cycle(List(Coin, Turn), 8)
    simulateMachine(inputs).run(Machine(true, 5, 10))
  }

  def modifyHelper (i: Input): Machine => Machine =
    (m: Machine) => applyInput(m)(i)

  def applyInput = (s: Machine) => (i: Input) => (i,s) match {
    case (_, Machine(_, 0, _)) => s
    case (Coin, Machine(true, candies, coins)) =>
      Machine(false, candies, coins + 1)
    case (Coin, Machine(false, _, _)) => s
    case (Turn, Machine(true, _, _)) => s
    case (Turn, Machine(false, candies, coins)) =>
      Machine(true, candies - 1, coins)
  }

}
