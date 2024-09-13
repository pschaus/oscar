package oscar.cp.test

import oscar.cp.testUtils.TestSuite
import org.scalatest.FunSuite
import oscar.algo.reversible.ReversibleDisjointSets
import oscar.cp.core.CPSolver

class TestReversibleDisjointSets extends TestSuite {

  test("findSet") {

    val nElems = 5
    val Elems = 0 until nElems
    val cp = CPSolver()
    val sets = ReversibleDisjointSets(cp, 5)

    for (i <- Elems; j <- Elems; if j != i) {
      sets.findSet(i) != sets.findSet(j) should be(true)
    }
  }

  test("sameSet") {

    val nElems = 5
    val Elems = 0 until nElems
    val cp = CPSolver()
    val sets = ReversibleDisjointSets(cp, 5)

    for (i <- Elems; j <- Elems; if j != i) {
      sets.sameSet(i, j) should be(false)
    }
  }

  test("union") {

    val nElems = 5
    val Elems = 0 until nElems
    val cp = CPSolver()
    val sets = ReversibleDisjointSets(cp, 5)

    val s1 = Set(0, 3)
    val s2 = Set(1, 2)
    val s3 = Set(0, 1, 2, 3)

    sets.union(0, 3)
    for (i <- Elems; j <- Elems; if j != i) {
      sets.sameSet(i, j) should be(s1.contains(i) && s1.contains(j))
    }

    sets.union(1, 2)
    for (i <- Elems; j <- Elems; if j != i) {
      sets.sameSet(i, j) should be(s2.contains(i) && s2.contains(j) || s1.contains(i) && s1.contains(j))
    }

    sets.union(2, 3)
    for (i <- Elems; j <- Elems; if j != i) {
      sets.sameSet(i, j) should be(s3.contains(i) && s3.contains(j))
    }
  }

  test("reversible") {

    val nElems = 5
    val Elems = 0 until nElems
    val cp = CPSolver()
    val sets = ReversibleDisjointSets(cp, 5)

    val s1 = Set(0, 3)
    val s2 = Set(1, 3)

    cp.pushState()
    for (i <- Elems; j <- Elems; if j != i) {
      sets.findSet(i) != sets.findSet(j) should be(true)
    }
    sets.union(0, 3)
    for (i <- Elems; j <- Elems; if j != i) {
      sets.sameSet(i, j) should be(s1.contains(i) && s1.contains(j))
    }
    cp.pop()
    for (i <- Elems; j <- Elems; if j != i) {
      sets.findSet(i) != sets.findSet(j) should be(true)
    }
    sets.union(1, 3)
    for (i <- Elems; j <- Elems; if j != i) {
      sets.sameSet(i, j) should be(s2.contains(i) && s2.contains(j))
    }
    
  }
}
