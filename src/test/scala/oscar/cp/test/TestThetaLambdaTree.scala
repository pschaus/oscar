package oscar.cp.test

/**
 * Created by saschavancauwelaert on 17/12/14.
 */


import org.scalatest.FunSuite
import oscar.cp.testUtils.TestSuite
import oscar.cp._
import oscar.cp.scheduling.util.ThetaLambdaTree

/**
 * Created by saschavancauwelaert on 17/12/14.
 */
class TestThetaLambdaTree extends TestSuite {

  test("unary unit fig2.8 of petr vilim's thesis") {
    implicit val cp = CPSolver()
    val starts : Array[CPIntVar] = Array(CPIntVar(0 to 100),CPIntVar(25 to 100),CPIntVar(30 to 100),CPIntVar(32 to 100))
    val durs = Array(CPIntVar(5),CPIntVar(6),CPIntVar(4),CPIntVar(10))
    val ends = Array(CPIntVar(0 to 5),CPIntVar(0 to 31),CPIntVar(0 to 34),CPIntVar(0 to 52))
    for (i <- 0 until starts.length) {
      cp.add(starts(i) + durs(i) === ends(i))
    }

    val tlTree = new ThetaLambdaTree(starts.length)

    tlTree.clearAndPlaceLeaves(Array.tabulate(starts.length)(i => i), starts.map(_.min), durs.map(_.min))

    tlTree.insert(0)
    tlTree.ect should be(5)
    tlTree.sumDuration should be(5)

    tlTree.insert(1)
    tlTree.ect should be(math.max(31, 11))
    tlTree.sumDuration should be(11)

    tlTree.clearAndPlaceLeaves(Array.tabulate(starts.length)(i => i), starts.map(_.min), durs.map(_.min))

    tlTree.insert(2)
    tlTree.insert(3)

    tlTree.ect should be(44)
    tlTree.sumDuration should be(14)

    tlTree.insert(0)
    tlTree.insert(1)

    val ectLeftAugmented = 31 + 14
    tlTree.ect should be(math.max(44 , ectLeftAugmented))
    tlTree.sumDuration should be(25)

  }

  test("unary unit fig2.9 of petr vilim's thesis") {
    implicit val cp = CPSolver()
    val starts : Array[CPIntVar] = Array(CPIntVar(0 to 100),CPIntVar(25 to 100),CPIntVar(30 to 100),CPIntVar(32 to 100))
    val durs = Array(CPIntVar(5),CPIntVar(9),CPIntVar(5),CPIntVar(10))
    val ends = Array(CPIntVar(0 to 100),CPIntVar(0 to 100),CPIntVar(0 to 100),CPIntVar(0 to 100))
    for (i <- 0 until starts.length) {
      cp.add(starts(i) + durs(i) === ends(i))
    }

    val tlTree = new ThetaLambdaTree(starts.length)

    tlTree.fillAndPlaceLeaves(Array.tabulate(starts.length)(i => i), starts.map(_.min), durs.map(_.min))

    tlTree.ect should be(49)
    tlTree.sumDuration should be(29)
    tlTree.ectBar should be(49)
    tlTree.sumDurationBar should be(29)

    tlTree.grayActivity(2)

    tlTree.ect should be(44)
    tlTree.sumDuration should be(24)
    tlTree.ectBar should be(49)
    tlTree.sumDurationBar should be(29)

    tlTree.responsibleEctBar should be(2)

  }


}
