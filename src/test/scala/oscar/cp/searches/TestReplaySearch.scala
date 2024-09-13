package oscar.cp.searches

import oscar.algo.search._
import oscar.cp._
import oscar.cp.testUtils._

class TestReplaySearch extends TestSuite {

  test("replay queens") {

    implicit val cp = CPSolver()
    val nQueens = 10
    // Number of queens
    val Queens = 0 until nQueens

    // Variables
    val queens = Array.fill(nQueens)(CPIntVar.sparse(0, nQueens - 1))

    // Search heuristic
    search(binaryFirstFail(queens))

    val allDiffs = Seq(allDifferent(queens),
                       allDifferent(Queens.map(i => queens(i) + i)),
                       allDifferent(Queens.map(i => queens(i) - i)))



    val linearizer = new DFSLinearizer()


    // Execution with FC allDifferent
    val statsInit = startSubjectTo(searchListener = linearizer) {
      add(allDiffs,Weak)
    }

    // Replay with AC allDifferent
    val statsReplayAC = cp.replaySubjectTo(linearizer,queens) {
      add(allDiffs,Strong)
    }

    assert(statsInit.nSols == statsReplayAC.nSols)
    assert(statsInit.nFails > statsReplayAC.nFails)


  }



}