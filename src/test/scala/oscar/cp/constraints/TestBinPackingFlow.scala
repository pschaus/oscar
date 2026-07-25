package oscar.cp.constraints

import oscar.cp.testUtils._

import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.constraints.{BinPackingFlow, EqCons}
import oscar.cp.testUtils.TestSuite

class TestBinPackingFlow extends TestSuite {

  test("test3") {
    val cp = CPSolver()
    val x = Array.tabulate(3)(_ => CPIntVar(0 to 1)(cp))
    val w = Array(6, 5, 4)
    val l = Array(CPIntVar(0 to 9)(cp), CPIntVar(0 to 6)(cp))

    val c = Array(CPIntVar(0 to 3)(cp), CPIntVar(0 to 3)(cp))

    try {
      cp.add(new BinPackingFlow(x, w, l, c))
      cp.add(new EqCons(x(0), 0))
    } catch {
      case _: Exception => // expected
    }

    cp.isFailed should be(true)
  }
}
