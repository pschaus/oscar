package oscar.cp.constraints

import oscar.cp.testUtils._

import oscar.algo.Inconsistency
import oscar.algo.reversible.SparseSet
import oscar.cp.constraints.EqCons
import oscar.cp.constraints.Sequence
import oscar.cp.core.CPStore
import oscar.cp.core.variables.CPIntVar
import org.scalatest.funsuite.AnyFunSuite

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestSequence extends AnyFunSuite {

  test("testSequence1") {
    implicit val s: CPStore = new CPStore()
    val x = Array.tabulate(4)(_ => CPIntVar(0, 1))
    val set = new SparseSet(1, 1)

    s.add(new EqCons(x(2), 0))
    s.add(new Sequence(x, set, 2, 1, 2))

    assert(x(3).isBound)
    assert(x(1).isBound)
    assert(x(1).min == 1)
    assert(x(3).min == 1)
    assert(!s.isFailed)
  }

  test("testSequence2") {
    implicit val s: CPStore = new CPStore()
    val x = Array.tabulate(4)(_ => CPIntVar(1, 5))
    val set = new SparseSet(3, 3)

    s.add(new EqCons(x(2), 2))
    s.add(new Sequence(x, set, 2, 1, 2))

    assert(x(3).isBound)
    assert(x(1).isBound)
    assert(x(1).min == 3)
    assert(x(3).min == 3)
    assert(!s.isFailed)
  }

  test("testSequence3") {
    implicit val s: CPStore = new CPStore()
    val x = Array.tabulate(4)(_ => CPIntVar(1, 5))
    val set = new SparseSet(2, 3)

    s.add(new EqCons(x(2), 1))
    s.add(new Sequence(x, set, 2, 1, 2))

    assert(x(1).getSize == 2 && x(1).hasValue(2) && x(1).hasValue(3))
    assert(x(3).getSize == 2 && x(3).hasValue(2) && x(3).hasValue(3))
    assert(!s.isFailed)
  }

  test("testSequence4") {
    implicit val s: CPStore = new CPStore()
    try {
      val x = Array.tabulate(4)(_ => CPIntVar(1, 5))
      val set = new SparseSet(4, 4)

      s.add(new EqCons(x(2), 1))
      s.post(new Sequence(x, set, 1, 1, 1))
      fail("Should have thrown Inconsistency")
    } catch {
      case e: Inconsistency => // ok
    }
  }

}
