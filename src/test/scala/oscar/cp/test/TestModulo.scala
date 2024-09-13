package oscar.cp.test

import org.scalatest.FunSuite
import oscar.cp.testUtils.TestSuite
import oscar.cp.constraints.DiffVal
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.CPStore

class TestModulo extends TestSuite {

  test("test modulo 1") {
    implicit val cp = new CPStore()
    val x = CPIntVar(List.apply(0, 3, 9, 12))
    val y = CPIntVar(0, 5)
    cp.post(x % 3 == y)
    assert(y.isBound)
    assert(y.min == 0)
  }

  test("test modulo 2") {
    implicit val cp = new CPStore()
    val x = CPIntVar(Set(0, 1, 6, 9, 12))
    val y = CPIntVar(0, 5)
    cp.post(x % 3 == y)
    assert(y.size == 2)
    cp.post(new DiffVal(x, 1))
    assert(y.isBound)
    assert(y.min == 0)
  }

  test("test modulo 3") {
    implicit val cp = new CPStore()
    val x = CPIntVar(Set(0, 1, 6, 9, 12))
    val y = CPIntVar(0, 5)
    cp.post(x % 3 == y)
    cp.post(new DiffVal(y, 0))
    assert(x.isBound)
    assert(x.min == 1)
  }

  test("test modulo 4") {
    implicit val cp = new CPStore()
    val x = CPIntVar(Set(0, 1, 6, 2, 9, 12))
    val y = CPIntVar(0, 5)
    cp.post(x % 3 == y)
    cp.post(new DiffVal(y, 0))
    cp.post(new DiffVal(y, 2))
    assert(x.isBound)
    assert(x.min == 1)
    assert(y.isBound)
    assert(y.min == 1)
  }

  test("test modulo 5") {
    implicit val cp = new CPStore()
    val x = CPIntVar(Set(0, -1, -6, -2, -9, -12))
    val y = CPIntVar(-5, 5)
    cp.post(x % 3 == y)
    cp.post(new DiffVal(y, 0))
    cp.post(new DiffVal(y, -2))
    assert(x.isBound)
    assert(x.min == -1)
    assert(y.isBound)
    assert(y.min == -1)
  }

  test("test modulo 6") {
    implicit val cp = new CPStore()
    val x = CPIntVar(Set(-6, -3, -9, -12, 3, 6, 9, 12))
    val y = CPIntVar(-5, 5)
    cp.post(x % 3 == y)
    assert(y.min == 0)
  }
  
  test("test modulo 7") {
    implicit val cp = new CPStore()
    val x = CPIntVar(0 until 10)
    val y = CPIntVar(Set(0, 2))
    cp.post(x % 3 == y)
    x.hasValue(1) should be(false)
    x.hasValue(4) should be(false)
    x.hasValue(7) should be(false)
  }
}
