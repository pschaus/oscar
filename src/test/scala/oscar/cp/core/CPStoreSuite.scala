package oscar.cp.core

import oscar.cp.testUtils.TestSuite
import oscar.cp.core.variables.CPIntVar
import oscar.cp.isInconsistent

class CPStoreSuite extends TestSuite {

  test("inconsistent assign should make the store inconsistent") {
    val store = new CPStore()
    val variable = CPIntVar(1 to 10)(store)
    assert(isInconsistent(store.assign(variable, 11)))
    assert(store.isFailed)
  }
  
  test("inconsistent remove should make the store inconsistent") {
    val store = new CPStore()
    val variable = CPIntVar(1 to 1)(store)
    assert(isInconsistent(store.remove(variable, 1)))
    assert(store.isFailed)
  }
}