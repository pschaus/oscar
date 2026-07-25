package oscar.cp.constraints

import oscar.cp.testUtils._

import oscar.cp._
import oscar.cp.constraints.Deviation
import oscar.cp.testUtils.TestSuite

class TestDeviation extends TestSuite {

  test("testDeviation1") {
    val cp = CPSolver()
    val x = Array.tabulate(4)(_ => CPIntVar(-2 to 2)(cp))
    val nd = CPIntVar(0 to 0)(cp)
    cp.add(new Deviation(x, 0, nd))
    cp.isFailed should be(false)
    for (i <- x.indices) {
      x(i).isBound should be(true)
      x(i).getMin should be(0)
    }
  }

  test("testDeviation2") {
    val cp = CPSolver()
    val x = Array.tabulate(4)(_ => CPIntVar(-2 to 2)(cp))
    val nd = CPIntVar(0 to 6)(cp)
    cp.add(new Deviation(x, 1, nd))
    cp.isFailed should be(false)
  }

  test("testDeviation3") {
    val cp = CPSolver()
    val x = new Array[CPIntVar](4)
    x(0) = CPIntVar(3 to 7)(cp)
    x(1) = CPIntVar(0 to 5)(cp)
    x(2) = CPIntVar(5 to 6)(cp)
    x(3) = CPIntVar(5 to 7)(cp)

    val nd = CPIntVar(0 to 18)(cp)
    cp.add(new Deviation(x, 17, nd))
    cp.isFailed should be(false)
    x(0).getMax should be(5)
  }

  test("testDeviation4") {
    val cp = CPSolver()
    val x = new Array[CPIntVar](4)
    x(0) = CPIntVar(3 to 7)(cp)
    x(1) = CPIntVar(0 to 5)(cp)
    x(2) = CPIntVar(5 to 6)(cp)
    x(3) = CPIntVar(5 to 7)(cp)

    val nd = CPIntVar(0 to 12)(cp)
    cp.add(new Deviation(x, 17, nd))
    cp.isFailed should be(false)
    x(0).getMax should be(4)
  }

  test("testDeviation5") {
    val cp = CPSolver()
    val x = new Array[CPIntVar](4)
    x(0) = CPIntVar(3 to 10)(cp)
    x(1) = CPIntVar(4 to 5)(cp)
    x(2) = CPIntVar(3 to 6)(cp)
    x(3) = CPIntVar(0 to 2)(cp)

    val nd = CPIntVar(0 to 45)(cp)
    cp.add(new Deviation(x, 17, nd))
    cp.isFailed should be(false)
    x(0).getMin should be(4)
    x(0).getMax should be(9)
  }

  test("testDeviation6") {
    val cp = CPSolver()
    val x = new Array[CPIntVar](4)
    x(0) = CPIntVar(3 to 10)(cp)
    x(1) = CPIntVar(4 to 5)(cp)
    x(2) = CPIntVar(3 to 6)(cp)
    x(3) = CPIntVar(0 to 2)(cp)

    val nd = CPIntVar(0 to 22)(cp)
    cp.add(new Deviation(x, 17, nd))
    cp.isFailed should be(false)
    x(0).getMin should be(4)
    x(0).getMax should be(7)
  }

  test("testDeviation7") {
    val cp = CPSolver()
    val x = new Array[CPIntVar](4)
    x(0) = CPIntVar(3 to 10)(cp)
    x(1) = CPIntVar(4 to 5)(cp)
    x(2) = CPIntVar(3 to 6)(cp)
    x(3) = CPIntVar(0 to 2)(cp)

    val nd = CPIntVar(0 to 30)(cp)
    cp.add(new Deviation(x, 17, nd))
    cp.isFailed should be(false)
    x(0).getMin should be(4)
    x(0).getMax should be(8)
  }

  test("testDeviation8") {
    val cp = CPSolver()
    val x = new Array[CPIntVar](4)
    x(0) = CPIntVar(4 to 5)(cp)
    x(1) = CPIntVar(3 to 6)(cp)
    x(2) = CPIntVar(0 to 2)(cp)
    x(3) = CPIntVar(3 to 10)(cp)

    val nd = CPIntVar(0 to 30)(cp)
    cp.add(new Deviation(x, 17, nd))
    cp.isFailed should be(false)
    x(3).getMin should be(4)
    x(3).getMax should be(8)
  }

  test("testDeviation9") {
    val cp = CPSolver()
    val x = new Array[CPIntVar](6)
    x(0) = CPIntVar(11 to 16)(cp)
    x(1) = CPIntVar(9 to 11)(cp)
    x(2) = CPIntVar(12 to 14)(cp)
    x(3) = CPIntVar(13 to 14)(cp)
    x(4) = CPIntVar(10 to 12)(cp)
    x(5) = CPIntVar(12 to 15)(cp)

    val nd = CPIntVar(0 to 1000)(cp)
    cp.add(new Deviation(x, 74, nd))
    cp.isFailed should be(false)
    nd.getMin should be(24)
  }

  test("testDeviation11") {
    val cp = CPSolver()
    val x = new Array[CPIntVar](8)
    x(0) = CPIntVar(-27 to -25)(cp)
    x(1) = CPIntVar(-27 to -27)(cp)
    x(2) = CPIntVar(-27 to -25)(cp)
    x(3) = CPIntVar(-27 to -25)(cp)
    x(4) = CPIntVar(-30 to -30)(cp)
    x(5) = CPIntVar(-27 to -25)(cp)
    x(6) = CPIntVar(-27 to -25)(cp)
    x(7) = CPIntVar(-27 to -23)(cp)

    val nd = CPIntVar(0 to 75)(cp)
    cp.add(new Deviation(x, -213, nd))
    cp.isFailed should be(false)
    x(7).getMax should be(-24)
  }

  test("testDeviation12") {
    val cp = CPSolver()
    val x = new Array[CPIntVar](6)
    x(0) = CPIntVar(11 to 16)(cp)
    x(1) = CPIntVar(9 to 11)(cp)
    x(2) = CPIntVar(12 to 14)(cp)
    x(3) = CPIntVar(13 to 14)(cp)
    x(4) = CPIntVar(10 to 12)(cp)
    x(5) = CPIntVar(12 to 15)(cp)

    val nd = CPIntVar(0 to 34)(cp)
    cp.add(new Deviation(x, 74, nd))
    cp.isFailed should be(false)
    x(0).getMax should be(14)
  }

  test("testDeviation13") {
    val cp = CPSolver()
    val x = new Array[CPIntVar](6)
    x(0) = CPIntVar(-14 to -12)(cp)
    x(1) = CPIntVar(-11 to -9)(cp)
    x(2) = CPIntVar(-14 to -12)(cp)
    x(3) = CPIntVar(-14 to -13)(cp)
    x(4) = CPIntVar(-12 to -10)(cp)
    x(5) = CPIntVar(-14 to -12)(cp)

    val nd = CPIntVar(0 to 34)(cp)
    cp.add(new Deviation(x, -74, nd))
    cp.isFailed should be(false)
    x(1).getMax should be(-10)
  }
}
