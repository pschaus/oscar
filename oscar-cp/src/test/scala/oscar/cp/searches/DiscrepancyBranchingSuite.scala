package oscar.cp.searches

import oscar.cp.testUtils._
import oscar.algo.search._
import oscar.algo.reversible.ReversibleInt

class DiscrepancyBranchingSuite extends TestSuite {

  test("test discrepancy 1") {
    val node = new DFSearchNode()
    val i = new ReversibleInt(node, 0)
    node.search {
      if (i > 0) noAlternative
      else branchAll(1 to 3) { v => i += 1 }
    }
    val stat = node.start(maxDiscrepancy = 1)
    assert(stat.nSols == 2)
  }

  test("test discrepancy 2") {
    val node = new DFSearchNode()
    val i = new ReversibleInt(node, 0)
    node.search {
      if (i > 2) noAlternative
      else branchAll(1 to 3) { v => i += 1 }
    }
    val stat = node.start(maxDiscrepancy = 5)
    assert(stat.nSols == 26)
  }

}