package oscar.cp.core

import oscar.cp.testUtils._

import oscar.cp.core.CPStore
import oscar.cp.core.variables.CPBoolVar
import oscar.cp.core.variables.CPIntVar
import org.scalatest.funsuite.AnyFunSuite

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestView extends AnyFunSuite {

  test("testView") {
    implicit val s: CPStore = new CPStore()
    val x = CPIntVar(1, 5)
    val y = CPIntVar(1, 5)
    println(x)

    val b = CPBoolVar()

    val x1 = oscar.cp.modeling.constraint.plus(x, 0)
    val x2 = oscar.cp.modeling.constraint.plus(x1, y)
    
    val x3 = oscar.cp.modeling.constraint.plus(x, 4)
    
    assert(!s.isFailed)
  }

}
