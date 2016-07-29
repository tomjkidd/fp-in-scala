package repl.prop

import testing._
import testing.Gen._
import testing.Prop._

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

  def exercise8Dot15() = {
    Prop.run(Prop.forAllBool(p => (p && !p) == false))
    Prop.run(Prop.forAllBool(p => (p && p) == p))
    Prop.run(Prop.forAllBool(p => (p || !p) == true))
  }
}
