package oscar.cp.core

import oscar.cp.testUtils._

import oscar.algo.Inconsistency
import oscar.cp.constraints.EqCons
import oscar.cp.constraints.GrEq
import oscar.cp.core.CPStore
import oscar.cp.core.variables.CPIntVar
import org.scalatest.funsuite.AnyFunSuite

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestGrAbs extends AnyFunSuite {

  test("testGrAbs") {
    implicit val s: CPStore = new CPStore()
    val x = Array.tabulate(2)(_ => CPIntVar(1, 256))
    
    val tmp = oscar.cp.modeling.constraint.absolute(oscar.cp.modeling.constraint.minus(x(0), x(1)))
    
    s.post(new GrEq(tmp, 0))
    s.post(new EqCons(x(0), 1))
    
    assert(!s.isFailed)
  }

}
