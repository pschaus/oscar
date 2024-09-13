package oscar.cp.test

import org.scalatest.FunSuite
import oscar.cp.testUtils.TestSuite
import oscar.cp._
import oscar.cp.scheduling.constraints.CumulativeDecomp

class TestCumulativeDecomp extends TestSuite {

  private class CPSched(durationsData: Array[Int], demandsData: Array[Int], horizon: Int) extends CPSolver {
    implicit val solver = this
    val nTasks = demandsData.size
    val Tasks = 0 until nTasks
    val durations = Array.tabulate(nTasks)(t => CPIntVar(durationsData(t)))
    val starts = Array.tabulate(nTasks)(t => CPIntVar(0 to horizon - durations(t).min))
    val ends = Array.tabulate(nTasks)(t => CPIntVar(durations(t).min to horizon))
    val demands = Array.tabulate(nTasks)(t => CPIntVar(demandsData(t)))
    val resources = Array.fill(nTasks)(CPIntVar(0))
    Tasks.foreach(t => this.post(ends(t) === starts(t) + durations(t)))
  }

  private def cumulative(cp: CPSched, capacity: Int): CumulativeDecomp = {
    new CumulativeDecomp(cp.starts, cp.durations, cp.ends, cp.demands, cp.resources, CPIntVar(capacity)(cp), 0)
  }

  test("solve all 1") {
    val durationsData = Array(2, 2, 2)
    val demandsData = Array(1, 1, 2)
    val horizon = 5
    val capacity = 2
    val cp = new CPSched(durationsData, demandsData, horizon)
    cp.add(cumulative(cp, capacity))

    val allSols = Set(
      List(0, 0, 2),
      List(0, 0, 3),
      List(1, 0, 3),
      List(0, 1, 3),
      List(1, 1, 3),
      List(3, 3, 1),
      List(2, 2, 0),
      List(3, 2, 0),
      List(2, 3, 0),
      List(3, 3, 0))

    var nSol = 0
    
    cp.search(binaryFirstFail(cp.starts)) 
    
    cp.onSolution {
      nSol += 1
      val sol = cp.starts.map(_.value).toList
      allSols contains sol should be(true)
    }
    
    cp.start()
    nSol should be(10)
  }

  test("fail 1") {
    val durationsData = Array(2, 2, 2)
    val demandsData = Array(2, 2, 2)
    val horizon = 5
    val capacity = 2
    val cp = new CPSched(durationsData, demandsData, horizon)
    cp.add(cumulative(cp, capacity))

    var nSol = 0
    cp.search(binaryFirstFail(cp.starts))
    cp.onSolution { nSol += 1 }
    assert(nSol == 0)
  }

}