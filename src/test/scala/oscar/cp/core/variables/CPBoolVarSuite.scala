package oscar.cp.core.variables

import oscar.cp.testUtils._
import oscar.cp.isInconsistent
import scala.util.Random
import oscar.cp.core.CPStore

class CPBoolVarSuite extends TestSuite {
  
  test("updateMin should update the domain if necessary") {
    val store = new CPStore()
    val b1 = CPBoolVar()(store)
    val b2 = CPBoolVar(true)(store)
    b1.updateMin(1)
    assert(b1.min == 1)
    b2.updateMin(1)
  }
  
  test("updateMax should update the domain if necessary") {
    val store = new CPStore()
    val b1 = CPBoolVar()(store)
    val b2 = CPBoolVar(false)(store)
    b1.updateMax(0)
    assert(b1.max == 0)
    b2.updateMax(0)
  }
  
  test("updateMin should fail if the value is greater than 1") {
    val store = new CPStore()
    val b = CPBoolVar()(store)
    assert(isInconsistent(b.updateMin(2)))
  }
  
  test("updateMax should fail if the value is lower than 0") {
    val store = new CPStore()
    val b = CPBoolVar()(store)
    assert(isInconsistent(b.updateMax(-1)))
  }
  
  test("updateMin should fail if the value is greater than the assigned value") {
    val store = new CPStore()
    val b = CPBoolVar(false)(store)
    assert(isInconsistent(b.updateMin(1)))
  }
  
  test("updateMax should fail if the value is lower than the assigned value") {
    val store = new CPStore()
    val b = CPBoolVar(true)(store)
    assert(isInconsistent(b.updateMax(0)))
  }
}