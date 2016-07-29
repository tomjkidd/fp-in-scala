import repl.state
import repl.par

import org.scalacheck.{Prop => SCProp, Gen => SCGen}

object PropRepl {
  def example1 () = {
    val intList = SCGen.listOf(SCGen.choose(0,100))

    val prop = SCProp.forAll(intList)(ns => ns.reverse.reverse == ns)
    prop
  }

  /* EXERCISE 8.1 */

  def exercise8Dot1 () = {
    val intList = SCGen.listOf(SCGen.choose(0,100))

    val prop1 = SCProp.forAll(intList)(ns => ns.sum == ns.reverse.sum)

    prop1.check
  }
}

