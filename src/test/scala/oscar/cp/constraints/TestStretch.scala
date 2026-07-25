package oscar.cp.constraints

import oscar.cp.testUtils._

import oscar.algo.Inconsistency
import oscar.cp.constraints.Automaton
import oscar.cp.constraints.EqCons
import oscar.cp.constraints.Regular
import oscar.cp.constraints.Stretch
import oscar.cp.core.CPStore
import oscar.cp.core.variables.CPIntVar
import org.scalatest.funsuite.AnyFunSuite

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestStretch extends AnyFunSuite {

  test("testStretch") {
    implicit val s: CPStore = new CPStore()
    val x = Array.tabulate(8)(_ => CPIntVar(0, 2))
    
    val shortest = Array(2, 2, 2)
    val longest = Array(4, 3, 2)
    
    val automaton: Automaton = Stretch.getStretchAutomaton(x, shortest, longest)
    s.post(new Regular(x, automaton))
    
    s.post(new EqCons(x(0), 0))
    assert(x(1).isBound && x(1).min == 0)
    
    s.post(new EqCons(x(1), 0))
    s.post(new EqCons(x(2), 0))
    s.post(new EqCons(x(3), 0))
    
    assert(!x(4).hasValue(0))
  }

}
