package oscar.cp.scheduling.precedencegraph

import org.scalatest.{Assertions, FunSuite, Matchers}
import oscar.cp.CPIntVar
import oscar.cp.core.CPSolver
import oscar.cp.core.variables.CPBoolVar

/**
  * Created by saschavancauwelaert on 16/01/2017.
  */
class PrecedenceGraphTest extends FunSuite with Matchers with Assertions {

  test("transitive closure DAG 3 nodes") {
    val nTasks = 3
    val horizon = 10
    implicit val cp = CPSolver()
    val starts = Array.fill(nTasks)(CPIntVar(0 until horizon))
    val durations = Array.fill(nTasks)(CPIntVar(2))
    val ends = Array.fill(nTasks)(CPIntVar(0 until horizon))
    val runOnResource = Array.fill(nTasks)(CPBoolVar(true))
    val initialKnownPrecedences : Array[(Int,Int)] = Array.ofDim(0)
    val graph = PrecedenceGraph(starts, durations, ends)

    graph.addNonDetectablePrecAndUpdateTransitiveClosure(2,1)

    assert(graph.hasNonDetectablePrecForPair(2,1))

    graph.addNonDetectablePrecAndUpdateTransitiveClosure(1,0)

    assert(graph.hasNonDetectablePrecForPair(1,0))
    assert(graph.hasNonDetectablePrecForPair(2,0))

  }

  test("transitive closure DAG 4 nodes") {
    val nTasks = 4
    val horizon = 10
    implicit val cp = CPSolver()
    val starts = Array.fill(nTasks)(CPIntVar(0 until horizon))
    val durations = Array.fill(nTasks)(CPIntVar(2))
    val ends = Array.fill(nTasks)(CPIntVar(0 until horizon))
    val runOnResource = Array.fill(nTasks)(CPBoolVar(true))
    val initialKnownPrecedences : Array[(Int,Int)] = Array.ofDim(0)
    val graph = PrecedenceGraph(starts, durations, ends)

    graph.addNonDetectablePrecAndUpdateTransitiveClosure(1,0)
    graph.addNonDetectablePrecAndUpdateTransitiveClosure(2,1)
    graph.addNonDetectablePrecAndUpdateTransitiveClosure(3,1)

    assert(graph.hasNonDetectablePrecForPair(2,0))
    assert(graph.hasNonDetectablePrecForPair(3,0))

  }

}
