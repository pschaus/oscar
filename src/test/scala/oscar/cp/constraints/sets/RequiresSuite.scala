package oscar.cp.constraints.sets

import oscar.cp.testUtils.TestSuite
import oscar.cp.core.variables.CPSetVar
import oscar.cp.core.CPSolver
import oscar.cp.core.variables.CPBoolVar

import org.scalatest.Assertions._

class RequiresSuite extends TestSuite {
  
  def newSet(elements: Set[Int]): (CPSolver, CPSetVar) = {
    val cp = CPSolver()
    (cp, CPSetVar(elements)(cp))
  }
  
  test("empty set") {
    intercept[Exception] { newSet(Set.empty) }
  }
  
  test("disjoint") {
    val (cp, set) = newSet(Set(1))
    val b = CPBoolVar()(cp)
    cp.post(Requires(set, 0, b))
    assert(b.isFalse)
    assert(!cp.isFailed)  
  }
  
  test("required") {
    val (cp, set) = newSet(Set(1, 2, 3))
    val b = CPBoolVar()(cp)
    cp.post(Requires(set, 1, b))
    assert(!b.isBound)
    assert(!set.isRequired(1))
    cp.post(set ++ 1)
    assert(b.isTrue)
    assert(!cp.isFailed)
  }
  
  test("excluded") {
    val (cp, set) = newSet(Set(1, 2, 3))
    val b = CPBoolVar()(cp)
    cp.post(Requires(set, 1, b))
    assert(!b.isBound)
    assert(!set.isRequired(1))
    cp.post(set -- 1)
    assert(b.isFalse)
    assert(!cp.isFailed)
  }
  
  test("reified (false)") {
    val (cp, set) = newSet(Set(1, 2, 3))
    val b = CPBoolVar()(cp)
    cp.post(Requires(set, 1, b))
    assert(!b.isBound)
    assert(!set.isRequired(1))
    cp.post(b.constraintFalse)
    assert(!set.isPossible(1))
    assert(!cp.isFailed)
  }
  
  test("reified (true)") {
    val (cp, set) = newSet(Set(1, 2, 3))
    val b = CPBoolVar()(cp)
    cp.post(Requires(set, 1, b))
    assert(!b.isBound)
    assert(!set.isRequired(1))
    cp.post(b.constraintTrue)
    assert(set.isRequired(1))
    assert(!cp.isFailed)
  }
}