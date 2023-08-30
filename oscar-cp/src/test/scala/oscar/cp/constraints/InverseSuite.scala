package oscar.cp.constraints

import oscar.cp.testUtils.TestSuite
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.CPSolver
import oscar.cp._

/** @author Renaud Hartert ren.hartert@gmail.com */
class InverseSuite extends TestSuite {
  
  test("Inverse should throw an error if its arrays do not have the same size") {
    val solver = CPSolver()
    val prev = Array.fill(5)(CPIntVar(0)(solver))
    val next = Array.fill(6)(CPIntVar(0)(solver))
    intercept[IllegalArgumentException] { solver.add(new Inverse(prev, next)) }  
  }

  test("Inverse should reduce the domains to the ids of its arrays") {
    val solver = CPSolver()
    val prev = Array.fill(5)(CPIntVar(-5 to 10)(solver))
    val next = Array.fill(5)(CPIntVar(0 to 5)(solver))
    solver.post(new Inverse(prev, next))
    assert(!solver.isFailed)
    for (p <- prev) {
      assert(p.min == 0)
      assert(p.max == 4)
    }
    for (n <- next) {
      assert(n.min == 0)
      assert(n.max == 4)
    }
  }
  
  test("Inverse should fail if an id does not appear in the domain of a variable") {
    val solver = CPSolver()
    val prev = Array.fill(5)(CPIntVar(0 to 4)(solver))
    val next = Array.fill(5)(CPIntVar(0 to 3)(solver))
    postAndCheckFailure(solver,new Inverse(prev, next))
  } 
  
  test("Inverse should fail if i is not in prev(j) and next(i) == j") {
    val solver = CPSolver()
    val prev = Array.fill(5)(CPIntVar(0 to 4)(solver))
    val next = Array.fill(5)(CPIntVar(0 to 4)(solver))
    solver.post(prev(3) !== 0)
    solver.post(next(0) === 3)
    postAndCheckFailure(solver, new Inverse(prev, next))
  }
  
  test("Inverse should fail if i is not in next(j) and prev(i) == j") {
    val solver = CPSolver()
    val prev = Array.fill(5)(CPIntVar(0 to 4)(solver))
    val next = Array.fill(5)(CPIntVar(0 to 4)(solver))
    solver.post(next(3) !== 0)
    solver.post(prev(0) === 3)
    postAndCheckFailure(solver, new Inverse(prev, next))
  }
  
  test("Inverse should assign prev(i) == j when next(j) == i") {
    val solver = CPSolver()
    val prev = Array.fill(5)(CPIntVar(0 to 4)(solver))
    val next = Array.fill(5)(CPIntVar(0 to 4)(solver))
    solver.post(new Inverse(prev, next))
    solver.post(next(0) === 3)
    assert(!solver.isFailed)
    assert(prev(3).isBound)
    assert(prev(3).min == 0) 
    solver.post(next(1) === 1)
    assert(!solver.isFailed)
    assert(prev(1).isBound)
    assert(prev(1).min == 1)
  }
  
  test("Inverse should assign next(j) == i when prev(i) == j") {
    val solver = CPSolver()
    val prev = Array.fill(5)(CPIntVar(0 to 4)(solver))
    val next = Array.fill(5)(CPIntVar(0 to 4)(solver))
    solver.post(new Inverse(prev, next))
    solver.post(prev(0) === 3)
    assert(!solver.isFailed)
    assert(next(3).isBound)
    assert(next(3).min == 0) 
    solver.post(prev(1) === 1)
    assert(!solver.isFailed)
    assert(next(1).isBound)
    assert(next(1).min == 1)
  }
  
  test("Inverse should remove i from next(j) when prev(i) != j") {
    val solver = CPSolver()
    val prev = Array.fill(5)(CPIntVar(0 to 4)(solver))
    val next = Array.fill(5)(CPIntVar(0 to 4)(solver))
    solver.post(new Inverse(prev, next))
    solver.post(prev(0) !== 3)
    assert(!next(3).hasValue(0)) 
    solver.post(prev(1) !== 1)
    assert(!next(1).hasValue(1)) 
  }
  
  test("Inverse should remove i from prev(j) when next(i) != j") {
    val solver = CPSolver()
    val prev = Array.fill(5)(CPIntVar(0 to 4)(solver))
    val next = Array.fill(5)(CPIntVar(0 to 4)(solver))
    solver.post(new Inverse(prev, next))
    solver.post(next(0) !== 3)
    assert(!prev(3).hasValue(0)) 
    solver.post(next(1) !== 1)
    assert(!prev(1).hasValue(1)) 
  }
}