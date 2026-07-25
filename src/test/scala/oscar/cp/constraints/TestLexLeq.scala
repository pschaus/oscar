package oscar.cp.constraints

import oscar.cp.testUtils._

import oscar.algo.Inconsistency
import oscar.cp.constraints.EqCons
import oscar.cp.constraints.LexLeq
import oscar.cp.core.CPStore
import oscar.cp.core.variables.CPIntVar
import org.scalatest.funsuite.AnyFunSuite

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestLexLeq extends AnyFunSuite {

  test("testLexLeq") {
    implicit val s: CPStore = new CPStore()
    
    val x = Array.tabulate(5)(_ => CPIntVar(0, 1))
    val y = Array.tabulate(5)(_ => CPIntVar(0, 1))

    s.post(new LexLeq(x, y))

    s.post(new EqCons(y(0), 0))
    s.post(new EqCons(y(1), 0))
    s.post(new EqCons(x(2), 1))

    println(s"${x(0)} ${x(1)} ${x(2)}")
    println(s"${y(0)} ${y(1)} ${y(2)}")
    
    assert(!s.isFailed)
    assert(x(0).min == 0)
    assert(x(1).min == 0)
    assert(y(2).min == 1)
  }
}
