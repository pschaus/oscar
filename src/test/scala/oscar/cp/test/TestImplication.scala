package oscar.cp.test

import org.scalatest.FunSuite
import oscar.cp.testUtils.TestSuite

import oscar.cp.constraints._

import oscar.cp._

class TestImplication extends TestSuite {

  test("=>1") {

    val cp = CPSolver()
    val A = CPBoolVar()(cp)
    val B = CPBoolVar()(cp)
    val res = A ==> B
    cp.add(res === 0)
    A.isBoundTo(1) should be(true)
    B.isBoundTo(0) should be(true)
  }

  test("=>2") {
    val values = Set((0, 0, 1), (0, 1, 1), (1, 0, 0), (1, 1, 1))
    val cp = CPSolver()
    val A = CPBoolVar()(cp)
    val B = CPBoolVar()(cp)
    val res = A ==> B
    cp.search {
      binaryStatic(Array(A, B))
    } onSolution {
      val entry = (A.value, B.value, res.value)
      values.contains(entry) should be(true)
    }
    cp.start().nSols should be(4)
  }

  test("=>3") {
    val values = Set((0, 0, 1), (0, 1, 1), (1, 0, 0), (1, 1, 1))
    val cp = CPSolver()
    val A = CPBoolVar()(cp)
    val B = CPBoolVar()(cp)
    val res = A ==> B
    cp.search {
      binaryStatic(Array(A, B))
    } onSolution {
      val entry = (A.value, B.value, res.value)
      values.contains(entry) should be(true)
    }
    cp.start().nSols should be(4)
  }

  test("=>4") {
    val values = Set((0, 0, 1), (0, 1, 1), (1, 0, 0), (1, 1, 1))
    val cp = CPSolver()
    val A = CPBoolVar()(cp)
    val B = CPBoolVar()(cp)
    val res = A ==> B
    var nbSol = 0
    cp.search {
      binaryStatic(Array(B, A))
    } onSolution {
      val entry = (A.value, B.value, res.value)
      values.contains(entry) should be(true)
    }
    cp.start().nSols should be(4)
  }

  test("=>5") {
    val values = Set((0, 0, 1), (0, 1, 1), (1, 0, 0), (1, 1, 1))
    val cp = CPSolver()
    val A = CPBoolVar()(cp)
    val B = CPBoolVar()(cp)
    val res = A ==> B
    cp.search {
      binaryStatic(Array(B, A))
    } onSolution {
      val entry = (A.value, B.value, res.value)
      values.contains(entry) should be(true)
    }
    cp.start().nSols should be(4)
  }

  test("=>6") {
    val cp = CPSolver()
    val A = CPBoolVar()(cp)
    val B = CPBoolVar()(cp)
    cp.add(A ==> B)
    cp.add(B === 0)
    A.isBoundTo(0) should be(true)
  }

  test("=>7") {
    val cp = CPSolver()
    val A = CPBoolVar()(cp)
    val B = CPBoolVar()(cp)
    cp.add(A === 1)
    cp.add(A ==> B)
    B.isBoundTo(1) should be(true)
  }
}
