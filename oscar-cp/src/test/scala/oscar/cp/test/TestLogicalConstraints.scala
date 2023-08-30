package oscar.cp.test

import org.scalatest.FunSuite
import oscar.cp.testUtils.TestSuite

import oscar.cp.constraints._

import oscar.cp._

class TestLogicalConstraints extends TestSuite {

  test("test logical 1") {

    val cp = CPSolver()
    var x = Array.fill(3)(CPBoolVar()(cp))

    cp.post((x(0) && x(1)) || x(2))

    cp.post(x(2) === 0)

    cp.isFailed should be(false)
    x(0).value should be(1)
    x(1).value should be(1)

  }

  test("test logical 2") {

    val cp = CPSolver()
    var x = Array.fill(3)(CPBoolVar()(cp))

    cp.post((x(0) && x(1)) || x(2))

    cp.post(x(0) === 0)

    cp.isFailed should be(false)
    x(2).value should be(1)
    x(1).isBound should be(false)

  }

  test("test logical 3") {

    val cp = CPSolver()
    var x = Array.fill(3)(CPBoolVar()(cp))

    cp.post((x(0) && x(1)) && x(2))

    cp.isFailed should be(false)
    x(0).value should be(1)
    x(1).value should be(1)
    x(2).value should be(1)

  }

  test("test logical 4") {

    val cp = CPSolver()

    val A = CPBoolVar()(cp)
    val B = CPBoolVar()(cp)
    val C = CPBoolVar()(cp)
    val D = CPBoolVar()(cp)

    cp.add(((A ==> B) || C) && D)

    D.isTrue should be(true)

    cp.add(A)
    cp.add(B === 0)

    C.isTrue should be(true)

  }

  test("test logical 5") {

    val cp = CPSolver()

    val w = CPIntVar(1 to 5)(cp)
    val x = CPIntVar(1 to 5)(cp)

    val A = w ?< x

    cp.add(A)

    w.max should be(4)

    cp.add(w ?< 2)
    w.max should be(1)

  }

  test("test logical 6") {

    val cp = CPSolver()

    val y = CPIntVar(1 to 5)(cp)
    val z = CPIntVar(1 to 5)(cp)

    cp.add(z ?> y)
    z.min should be(2)

    cp.add((z ?> 3).constraintFalse) // it means z <= 3
    z.max should be(3)

  }

  test("test logical 7") {

    val cp = CPSolver()

    val y = CPIntVar(1 to 5)(cp)
    val z = CPIntVar(3 to 5)(cp)
    val b = z ?=== y

    cp.add(y <= 2)
    b.value should be(0)

  }

  test("test logical 8") {

    val cp = CPSolver()

    val y = CPIntVar(Set(1, 5, 9))(cp)
    val z = CPIntVar(Set(5, 10, 13))(cp)
    val b = z ?=== y

    //println(b)

    b.isBound should be(false)

    cp.add(z !== 5)

    b.value should be(0)

  }

  test("test logical 9") {

    val cp = CPSolver()

    val y = CPIntVar(Set(1, 5, 9))(cp)
    val z = CPIntVar(Set(5, 10, 13))(cp)
    val b = z === y
    cp.add(b)

    y.value should be(5)
    z.value should be(5)

  }

  test("test logical 10") {

    val cp = CPSolver()

    val y = CPIntVar(Set(1, 5, 9))(cp)
    val z = CPIntVar(Set(5, 10, 13))(cp)
    val b = z ?=== y
    cp.add(b.constraintFalse)
    cp.add(z === 5)
    y.hasValue(5) should be(false)

  }

  test("test logical 11") {

    val cp = CPSolver()

    val y = CPIntVar(Set(1, 5, 9))(cp)
    val z = CPIntVar(Set(5, 10, 13))(cp)
    val b = z !== y
    cp.add(b)
    cp.add(z ?=== 5)
    y.hasValue(5) should be(false)

  }

  test("test logical 12") {

    val cp = CPSolver()

    val x = CPBoolVar()(cp)
    val y = CPBoolVar()(cp)
    cp.add(x || y)
    isInconsistent(cp.post(!x && !y)) should be(true)
    cp.isFailed should be(true)

  }



}
