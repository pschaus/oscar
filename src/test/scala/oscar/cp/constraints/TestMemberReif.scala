package oscar.cp.constraints

import oscar.cp.testUtils._

import oscar.algo.Inconsistency
import oscar.algo.reversible.SparseSet
import oscar.cp.constraints.GrEq
import oscar.cp.constraints.LeEq
import oscar.cp.constraints.MemberReif
import oscar.cp.core.CPStore
import oscar.cp.core.variables.CPBoolVar
import oscar.cp.core.variables.CPIntVar
import org.scalatest.funsuite.AnyFunSuite

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestMemberReif extends AnyFunSuite {

  test("test0") {
    implicit val s: CPStore = new CPStore()
    val set = new SparseSet(0, 10, true)
    set.insert(1)
    set.insert(2)
    set.insert(4)
    // set1 = {1,2,4}
    val x = CPIntVar(0, 5)
    // x = {0,1,2,3,4,5}
    val b = CPBoolVar()
    assert(!b.isBound)
    s.post(b.constraintFalse) // forbid x to be a member of set
    s.post(new MemberReif(x, set, b))
    assert(!s.isFailed)
    // check that x lost its values 1,2 and 4
    assert(!x.hasValue(1) && !x.hasValue(2) && !x.hasValue(4))
  }

  test("test1") {
    implicit val s: CPStore = new CPStore()
    val set = new SparseSet(0, 10, true)
    set.insert(1)
    set.insert(2)
    set.insert(4)
    val x = CPIntVar(0, 5)
    val b = CPBoolVar()
    s.post(new MemberReif(x, set, b))
    assert(!b.isBound)
    s.post(b.constraintFalse) // forbid x to be a member of set
    assert(!s.isFailed)
    assert(!x.hasValue(1) && !x.hasValue(2) && !x.hasValue(4))
  }

  test("test2") {
    implicit val s: CPStore = new CPStore()
    val set = new SparseSet(0, 10, true)
    set.insert(1)
    set.insert(2)
    set.insert(4)
    val x = CPIntVar(0, 5)
    val b = CPBoolVar()
    assert(!b.isBound)
    s.post(b.constraintTrue) // force x to be a member of s
    s.post(new MemberReif(x, set, b))
    assert(!s.isFailed)
    assert(x.hasValue(1) && x.hasValue(2) && x.hasValue(4) && x.getSize == 3)
  }

  test("test3") {
    implicit val s: CPStore = new CPStore()
    val set = new SparseSet(0, 10, true)
    set.insert(1)
    set.insert(2)
    set.insert(4)
    val x = CPIntVar(0, 5)
    val b = CPBoolVar()
    assert(!b.isBound)
    s.post(new MemberReif(x, set, b))
    s.post(b.constraintTrue) // force x to be a member of s
    assert(!s.isFailed)
    assert(x.hasValue(1) && x.hasValue(2) && x.hasValue(4) && x.getSize == 3)
  }

  test("test4") {
    implicit val s: CPStore = new CPStore()
    val set = new SparseSet(0, 10, true)
    set.insert(1)
    set.insert(2)
    set.insert(3)
    val x = CPIntVar(1, 5)
    val b = CPBoolVar()
    assert(!b.isBound)
    s.post(new MemberReif(x, set, b))
    s.post(new LeEq(x, 3)) // force D(x) = 1,2,3 so that x is always a member
    assert(!s.isFailed)
    assert(b.isTrue)
  }

  test("test5") {
    implicit val s: CPStore = new CPStore()
    val set = new SparseSet(0, 10, true)
    set.insert(1)
    set.insert(2)
    set.insert(3)
    val x = CPIntVar(1, 5)
    val b = CPBoolVar()
    assert(!b.isBound)
    s.post(new LeEq(x, 3)) // force D(x) = 1,2,3 so that x is always a member
    s.post(new MemberReif(x, set, b))
    assert(!s.isFailed)
    assert(b.isTrue)
  }

  test("test6") {
    implicit val s: CPStore = new CPStore()
    val set = new SparseSet(0, 10, true)
    set.insert(1)
    set.insert(2)
    set.insert(3)
    val x = CPIntVar(1, 5)
    val b = CPBoolVar()
    assert(!b.isBound)
    s.post(new GrEq(x, 4)) // force D(x) = 4,5 so that x is not a member
    s.post(new MemberReif(x, set, b))
    assert(!s.isFailed)
    assert(b.isFalse)
  }

  test("test7") {
    implicit val s: CPStore = new CPStore()
    val set = new SparseSet(0, 10, true)
    set.insert(1)
    set.insert(2)
    set.insert(3)
    val x = CPIntVar(1, 5)
    val b = CPBoolVar()
    assert(!b.isBound)
    s.post(new MemberReif(x, set, b))
    s.post(new GrEq(x, 4)) // force D(x) = 4,5 so that x is not a member
    assert(!s.isFailed)
    assert(b.isFalse)
  }

}
